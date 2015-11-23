## this is in 'already-wrapped' format

load("mergedData.RData")
options(width=200)
library("lme4")

## BMB: can we use broom/whisker instead??
library("coefplot2")
library("gdata")
library("splines")

# Make a special data frame for this model

predNames <- c( "knowsPersonHIV", "knowsCondomProtect")

modNames <- c("age", "gender", "urbanRural", "educLevel",
              "wealthRaw", "MrtStat", "religion",
              "AIDSlookHealthy", "whoLastSex")
factNames <- c("province", "clusterId", "dataset")
allNames <- c(predNames, modNames,
              factNames, "condomLastTime")

##  select columns and sub-sample
ModAns <- na.omit(Answers[allNames])

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

getComp <- function(obj,FUN) {
    if (is(obj,"merMod")) FUN(obj) else FUN(obj)$cond
}
modelSum <- function(x,time=NULL) {
    ff <- fixef(x)
    list(LL=-logLik(x),AIC=AIC(x),
         fixef=getComp(x,fixef),
         ci = if (is(x,"merMod")) confint(x,method="Wald") else confint(x,method="Wald"),
         ngrps = if (is(x,"merMod")) ngrps(x) else glmmTMB:::ngrps.glmmTMB(x)$cond,
         vv = getComp(x,VarCorr),
         time=time)
}


t_null_bobyqa <- system.time(
    null_model_bobyqa <- glmer(nullModForm,family="binomial",
                           data=rMod,
                           control=glmerControl(optimizer="bobyqa")))

knows_model <- update(null_model, . ~
	. + knowsPersonHIV + (0 + knowsPersonHIV | dataset))

avoid_model <- update(null_model, . ~
	. + knowsCondomProtect + (0 + knowsCondomProtect | dataset))

base_model <- update(avoid_model, . ~
	. + knowsPersonHIV + (0 + knowsPersonHIV | dataset))

interaction_model <- update(base_model, . ~ . +  knowsPersonHIV:knowsCondomProtect + (0 + knowsPersonHIV:knowsCondomProtect|dataset))

drop1(base_model, test="Chisq")

###################### Base Model ###############################
print(summary(base_model),correlation=FALSE)

############ Knowledge x PWA Interaction ######################
print(summary(interaction_model),correlation=FALSE)

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

save.image()

# rdsave(base_model, ModAns)

