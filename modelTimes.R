load("mergedData2.rda")
library("lme4")
library("glmmTMB")
library("splines")

## fvec <- 10^seq(-1.6,0,length=15)
fvec <- 10^seq(-1.6,-1.5,length=2)
res <- list(glmer=vector("list",length(fvec)),
            glmmTMB=vector("list",length(fvec)))
for (i in seq_along(fvec)) {
    cat(i,"\n")
    set.seed(101)
    rMod <- sampFun(round(fvec[i]*nrow(ModAns)))
    cat("   glmer ...\n")
    tt1 <- system.time(g1 <- glmer(nullModForm, 
                      family="binomial", data=rMod,
                      control=glmerControl(optimizer="bobyqa")))
    res$glmer[[i]] <- modelSum(g1,tt1)
    cat("   glmmTMB ...\n")
    tt2 <- system.time(g2 <- glmmTMB(nullModForm, 
                      family="binomial", data=rMod))
    res$glmmTMB[[i]] <- modelSum(g2,tt2)
    save("res",file="modelTimes.rda")
}

    
    
