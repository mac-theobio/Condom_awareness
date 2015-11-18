## this is in 'already-wrapped' format

load("mergedData.RData")
options(width=200)
library("lme4")
library("glmmTMB")

## BMB: can we use broom/whisker instead??
library("coefplot2")
library("gdata")
library("splines")


# Make a special data frame for this model

nObs <- 1500  ## 2.6% ; (1/37) of data
nObsMed <- round(nrow(ModAns)/20) ## 10% of data
nObsLong <- round(nrow(ModAns)/10) ## 10% of data

predNames <- c( "knowsPersonHIV", "knowsCondomProtect")

modNames <- c("age", "gender", "urbanRural", "educLevel",
              "wealthRaw", "MrtStat", "religion",
              "AIDSlookHealthy", "whoLastSex")
factNames <- c("province", "clusterId", "dataset")
allNames <- c(predNames, modNames,
              factNames, "condomLastTime")

##  select columns and sub-sample
ModAns <- na.omit(Answers[allNames])

sampFun <- function(n,d=ModAns) {
    d[sort(sample(nrow(d),n)),]
}

set.seed(101)
rMod <- sampFun(nObs)
rModMed <- sampFun(nObsMed)
rModLong <- sampFun(nObsLong)

nullModForm <- 	condomLastTime	~ ns(age, 4) +
    ns(wealthRaw, 4) +
        gender + 
	urbanRural + 
	educLevel + 
	religion  + 
	MrtStat + 
	AIDSlookHealthy + 
	whoLastSex + 
	(1|dataset:clusterId) + 
	(1|dataset:province) + 
	(1|dataset)

t_null_default <- system.time(null_model_default <- glmer(nullModForm, 
                      family="binomial", data=rMod
                      )
                      )
## default optimizer (c("bobyqa","Nelder_Mead"))
## nObs=500:
##   warns 'unable to evaluate scaled gradient'
##    time ~ 34 seconds
## nObs=1500: failed to converge in 10000 evals
## failed to converge with max |grad|= 0.53
## ~ 100 seconds

t_null_bobyqa <- system.time(
    null_model_bobyqa <- update(null_model_default,
                           control=glmerControl(optimizer="bobyqa")))

##   optimizer="bobyqa":
##  nObs=500: unable to evaluate scaled gradient,
##     degen Hessian with 3 negative eigs
## nObs=1500:  ~ 46 seconds, no warnings

t_null_nloptr <- system.time(
    null_model_nloptr <- update(null_model_default,
                      control=glmerControl(optimizer=nloptwrap)))

## nObs=1500: failed to converge with maxgrad 0.07
## ~ 24 seconds

all.equal(fixef(null_model_nloptr),
          fixef(null_model_bobyqa))  ## 1% difference

## t_null_nloptr2 <- system.time(
##     null_model_nloptr2 <- update(null_model_default,
##                    control=glmerControl(optimizer=nloptwrap,
##                            optCtrl=list(xtol_rel=1e-6,
##                                         ftol_abs=1e-6))))
                                 
## Would like to try decreasing tolerance to improve
## accuracy to see where we end up. These settings don't work, though ..


t_null_tmb <- system.time(null_model_g <- glmmTMB(nullModForm, 
                      family="binomial", data=rMod,
                      )
                      )
## nObs=500 fails in solve.default(hessian.fixed) after 4 seconds
## nObs=1500; 39 seconds *without* optimization, 17 seconds *with* optimization

t_med_tmb <- system.time(null_model_g_med <- update(null_model_g,data=rModMed))

## 10% sample of data; how does this scale?  230 seconds, ugh.
t_long_tmb <- system.time(null_model_g_long <- update(null_model_g,data=rModLong))

## for developers, would be nice to query/warn if glmmTMB was built
## with or without optimization ...

all.equal(fixef(null_model_bobyqa),
          fixef(null_model_g)[[1]],tol=1e-3)

## summarize models so we don't have to store multiple copies
## of the data ...

modStr <- ls(pattern="null_model_.*")
modList <- lapply(modStr,get)
tList <- lapply(ls(pattern="t_.*"),get)
names(modList) <- names(tList) <- gsub("_model_","_",modStr)

confint.glmmTMB(modList$null_g)
getComp <- function(obj,FUN) {
    if (is(obj,"merMod")) FUN(obj) else FUN(obj)$cond
}
modelSum <- function(x,time=NULL) {
    ff <- fixef(x)
    c(LL=-logLik(x),AIC=AIC(x),
      fixef=getComp(x,fixef),
      ## temporary hack
      ci = if (is(x,"merMod")) confint(x,method="Wald") else confint.glmmTMB(x,method="Wald"),
      vv = getComp(x,VarCorr),
      time=time)
}
          
sumList <- mapply(modelSum,modList,tList)

save("sumList",file="modSumList.rda")
q()


knows_model <- update(null_model, . ~
	. + knowsPersonHIV + (0 + knowsPersonHIV | dataset))

avoid_model <- update(null_model, . ~
	. + knowsCondomProtect + (0 + knowsCondomProtect | dataset))

base_model <- update(avoid_model, . ~
	. + knowsPersonHIV + (0 + knowsPersonHIV | dataset))

interaction_model <- update(base_model, . ~ . +  knowsPersonHIV:knowsCondomProtect + (0 + knowsPersonHIV:knowsCondomProtect|dataset))

drop1(base_model, test="Chisq")

###################### Base Model ###############################
lme4:::printMer(summary(base_model),correlation=FALSE)

############ Knowledge x PWA Interaction ######################
lme4:::printMer(summary(interaction_model),correlation=FALSE)

### Overall effect without interaction###
anova(base_model, null_model)

### Overall effect of interaction###
anova(base_model, interaction_model)

### Effect of "avoid" in base model ###
# Compare interaction model by dropping two "avoid" terms
anova(knows_model, base_model)

### Effect of "knows" in base model ###
# As above
anova(avoid_model, base_model)

# rdsave(base_model, ModAns)

