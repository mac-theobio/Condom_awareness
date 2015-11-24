load("mergedData2.rda")
library("lme4")
library("glmmTMB")
library("splines")

fvec <- 10^seq(-1.6,0,length=15)
res <- list(glmmTMB_opt=vector("list",length(fvec)))
for (i in seq_along(fvec)) {
    cat(i,"\n")
    set.seed(101)
    rMod <- sampFun(round(fvec[i]*nrow(ModAns)))
    cat("   glmmTMB_opt ...\n")
    tt2 <- system.time(g2 <- glmmTMB(nullModForm, 
                      family="binomial", data=rMod))
    res$glmmTMB_opt[[i]] <- modelSum(g2,tt2)
    save("res",file="modelTimes3.rda")
}

    
    
