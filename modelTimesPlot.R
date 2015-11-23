load("mergedData2.rda")
library("plyr")
library("reshape2")
library("ggplot2"); theme_set(theme_bw())
library("gridExtra")
library("nlme")

pdf("modelTimesPlot.pdf")

n <- nrow(ModAns)

## sample sizes -- should be stored along with run info ...
fvec <- 10^seq(-1.6,0,length=15)
fvec2 <- round(n*fvec)

load("modelTimes.rda")
nComplete <- tail(which(sapply(res$glmer,function(x)!is.null(x))),1)
fvec3 <- fvec2[1:nComplete]
res0 <- res
res0 <- lapply(res0,"[",1:nComplete)

times <- ldply(res0,
               function(x) ldply(x,"[[","time"))
times <- ddply(times,".id",
               cbind,fvec3)

##  do it again for the second set of data (default optimizer, nloptr)

load("modelTimes2.rda")
nComplete <- tail(which(sapply(res$glmer_nloptr,function(x)!is.null(x))),1)
fvec3 <- fvec2[1:nComplete]
res <- lapply(res,"[",1:nComplete)
times2 <- ldply(res,
               function(x) ldply(x,"[[","time"))
times2 <- ddply(times2,".id",
               cbind,fvec3)
times <- rbind(times,times2)

## 1. check times


(gg_times <- ggplot(times,aes(fvec3,elapsed/3600,colour=.id))+
    geom_line()+geom_point()+
    scale_x_log10(breaks=c(2000,5000,10000,20000,50000))+
        scale_y_log10()+
            labs(x="data set size",y="hours"))

coef(lmList(log(elapsed)~log(fvec3)|.id,data=times))[,2]
## lme4 scales ~ 0.7-0.8; glmmTMB scales ~ 2

## 2. check n groups

ngrps <- ldply(res$glmer_default,"[[","ngrps")
nfix <- length(res$glmer_default[[1]]$fixef)
ngrps2 <- data.frame(nobs=fvec3,
                     nobs2=fvec3, ## for melting
                     nranef=rowSums(ngrps),
                     npar=rowSums(ngrps)+nfix,
                     mult=(rowSums(ngrps)+nfix)*fvec3)
ngrps_m <- melt(ngrps2,id.var="nobs")
ngrps_m <- ddply(ngrps_m,"variable",transform,
                 value=value/value[1])
(gg_ngrps <- ggplot(ngrps_m,aes(nobs,value,colour=variable))+
     geom_line()+geom_point()+
         scale_x_log10(breaks=c(2000,5000,10000,20000,50000))+
         scale_y_log10(limit=c(1,100),oob=scales::squish)+
            labs(x="data set size",y="relative size"))

## 3. check variation in results

names(res$glmer_default[[1]]$fixef)
## pick some parameters to check
chkParms <- c("genderM","urbanRuralRural","AIDSlookHealthyYes",
              "whoLastSexNon-cohabiting partner")

ngrps <- ldply(res$glmer_default,"[[","ngrps")

parms <- ldply(c(res0,res),
               function(x) ldply(x,
                 function(z) { r <- z$fixef
                               r
                           }))
parms <- ddply(parms,".id",
               function(x) data.frame(x,
                                      nobs=fvec3[1:length(x$genderM)]))
                                 
parms_m <- melt(parms,id.vars=c("nobs",".id"))
(gg_parms <- ggplot(subset(parms_m,variable %in% make.names(chkParms)),
                           aes(nobs,value,colour=.id))+
     geom_line()+geom_point()+
     scale_x_log10(breaks=c(2000,5000,10000,20000,50000))+
            labs(x="data set size",y="value")+
            facet_wrap(~variable,scales="free"))


parms2 <- dcast(parms_m,nobs+variable~.id)
parmsdiff <- ddply(parms2,c("nobs","variable"),
                   function(x) {
                       y <- na.omit(unlist(x[,-(1:2)]))
                       data.frame(rmse=sqrt(sum((y/mean(y)-1)^2)))
                   })
ggplot(parmsdiff,aes(x=nobs,y=rmse,colour=variable))+geom_point()+
    geom_line()+scale_y_log10(breaks=c(0.001,0.005,0.01,0.05,0.1))+
        scale_colour_discrete(guide=guide_legend(ncol=8,title.position="top"))+
            theme(legend.position="top")
            


parmsdiff2 <- ddply(parms2,c("nobs","variable"),
                   function(x) {
                       y <- na.omit(unlist(x[,-(1:2)]))
                       with(x,data.frame(variable=variable,rbind(c(nobs=nobs,
                                                 abs(y/median(y)-1)))))
                   })

parmsdiff2 <- rename(parmsdiff2,c(variable="param"))
parmsdiff2_m <- melt(parmsdiff2,id.vars=c("nobs","param"))

ggplot(parmsdiff2_m,aes(x=nobs,y=value,colour=param))+geom_point()+
    geom_line()+scale_y_log10(breaks=c(0.001,0.005,0.01,0.05,0.1))+
        facet_wrap(~variable)+
    geom_hline(yintercept=0.01,lty=2)+
    theme(panel.margin=grid::unit(0,"lines"))+
        scale_colour_discrete(guide=guide_legend(ncol=8,title.position="top"))+
            theme(legend.position="top")

          
dev.off()

## CONCLUSIONS:
##  plot 1 (times): glmmTMB elapsed time is scaling quadratically (why???),
##   lme4 is scaling as ~ nobs^0.75.  glmer is best with nloptr (~5 mins
##   for full data set), second-best with bobyqa (~12 minutes?), worst with
##   default (~30 minutes?)

## plot 2: even taking into account numbers of parameters (including #
## of random-effects levels) * nobs, I don't see why glmmTMB time should
##  scale quadratically ...

## plot 3: looking at a few haphazardly chosen parameters and how they
##  vary among fitting methods.  Answers don't differ *too* radically ...
##
## plot 4: RMSEs of all parameters across methods ...  mostly small,
##  but varies as high as about 20%

## plot 5: scaled distance of each parameter from the *median*.
##  looks like glmer with bobyqa (= 'glmer' alone) may be the best
## compromise between speed & accuracy ...




