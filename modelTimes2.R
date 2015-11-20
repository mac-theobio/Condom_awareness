load("mergedData2.rda")
library("lme4")
library("splines")

fvec <- 10^seq(-1.6,0,length=15)
##fvec <- 10^seq(-1.6,-1.5,length=2)
res <- list(glmer_default=vector("list",length(fvec)),
            glmer_nloptr=vector("list",length(fvec)))
for (i in seq_along(fvec)) {
    cat(i,"\n")
    set.seed(101)
    rMod <- sampFun(round(fvec[i]*nrow(ModAns)))
    cat("   glmer_default ...\n")
    tt1 <- system.time(g1 <- glmer(nullModForm, 
                      family="binomial", data=rMod))
    res$glmer_default[[i]] <- modelSum(g1,tt1)
    cat("   glmer_nloptr ...\n")
    tt2 <- system.time(g2 <- glmer(nullModForm, 
                      family="binomial", data=rMod,
                      control=glmerControl(optimizer="nloptwrap")))
    res$glmer_nloptr[[i]] <- modelSum(g2,tt2)
    save("res",file="modelTimes2.rda")
}

    
    
