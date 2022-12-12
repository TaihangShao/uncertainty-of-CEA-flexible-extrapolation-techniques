############################-----------------------------------------------###
########  input  ###########-----------------------------------------------###
############################-----------------------------------------------###
output0<-output_niv_os
############################-----------------------------------------------###
############################-----------------------------------------------###

dfGOF<-output0[[1]]
dfGOF$LnL<-as.numeric(dfGOF$LnL)
dfGOF$Params<-as.numeric(dfGOF$Params)
dfGOF$AIC <- -2*dfGOF$LnL + 2*dfGOF$Params
ltHaz<-output0[[2]]
dfHazEst<-output0[[3]]
dfHazEst2<-output0[[4]]
dfSurv<-output0[[5]]
dfSurv2<-output0[[6]]

fit_niv3y<-survfit(Surv(niv3y$recyrs,niv3y$censrec)~1,data=niv3y)
data_fit_niv3y<-data.frame(time=fit_niv3y$time, surv=fit_niv3y$surv,lower=fit_niv3y$lower,upper=fit_niv3y$upper)

fit_niv5y<-survfit(Surv(niv5y$recyrs,niv5y$censrec)~1,data=niv5y)
data_fit_niv5y<-data.frame(time=fit_niv5y$time, surv=fit_niv5y$surv,lower=fit_niv5y$lower,upper=fit_niv5y$upper)

source("3-further-output.R")

############################-----------------------------------------------###
########  input  ###########-----------------------------------------------###
############################-----------------------------------------------###
output0<-output_ipi_os
############################-----------------------------------------------###
############################-----------------------------------------------###

dfGOF<-output0[[1]]
dfGOF$LnL<-as.numeric(dfGOF$LnL)
dfGOF$Params<-as.numeric(dfGOF$Params)
dfGOF$AIC <- -2*dfGOF$LnL + 2*dfGOF$Params
ltHaz<-output0[[2]]
dfHazEst<-output0[[3]]
dfHazEst2<-output0[[4]]
dfSurv<-output0[[5]]
dfSurv2<-output0[[6]]

fit_niv3y<-survfit(Surv(ipi3y$recyrs,ipi3y$censrec)~1,data=ipi3y)
data_fit_niv3y<-data.frame(time=fit_niv3y$time, surv=fit_niv3y$surv,lower=fit_niv3y$lower,upper=fit_niv3y$upper)

fit_niv5y<-survfit(Surv(ipi5y$recyrs,ipi5y$censrec)~1,data=ipi5y)
data_fit_niv5y<-data.frame(time=fit_niv5y$time, surv=fit_niv5y$surv,lower=fit_niv5y$lower,upper=fit_niv5y$upper)

source("3-further-output.R")


############################-----------------------------------------------###
########  input  ###########-----------------------------------------------###
############################-----------------------------------------------###
output0<-output_niv_pfs
############################-----------------------------------------------###
############################-----------------------------------------------###

dfGOF<-output0[[1]]
dfGOF$LnL<-as.numeric(dfGOF$LnL)
dfGOF$Params<-as.numeric(dfGOF$Params)
dfGOF$AIC <- -2*dfGOF$LnL + 2*dfGOF$Params
ltHaz<-output0[[2]]
dfHazEst<-output0[[3]]
dfHazEst2<-output0[[4]]
dfSurv<-output0[[5]]
dfSurv2<-output0[[6]]

fit_niv3y<-survfit(Surv(niv3y$recyrs,niv3y$censrec)~1,data=niv3y)
data_fit_niv3y<-data.frame(time=fit_niv3y$time, surv=fit_niv3y$surv,lower=fit_niv3y$lower,upper=fit_niv3y$upper)

fit_niv5y<-survfit(Surv(niv5y$recyrs,niv5y$censrec)~1,data=niv5y)
data_fit_niv5y<-data.frame(time=fit_niv5y$time, surv=fit_niv5y$surv,lower=fit_niv5y$lower,upper=fit_niv5y$upper)


source("3-further-output.R")


############################-----------------------------------------------###
########  input  ###########-----------------------------------------------###
############################-----------------------------------------------###
output0<-output_ipi_pfs
############################-----------------------------------------------###
############################-----------------------------------------------###

dfGOF<-output0[[1]]
dfGOF$LnL<-as.numeric(dfGOF$LnL)
dfGOF$Params<-as.numeric(dfGOF$Params)
dfGOF$AIC <- -2*dfGOF$LnL + 2*dfGOF$Params
ltHaz<-output0[[2]]
dfHazEst<-output0[[3]]
dfHazEst2<-output0[[4]]
dfSurv<-output0[[5]]
dfSurv2<-output0[[6]]


fit_niv3y<-survfit(Surv(ipi3y$recyrs,ipi3y$censrec)~1,data=ipi3y)
data_fit_niv3y<-data.frame(time=fit_niv3y$time, surv=fit_niv3y$surv,lower=fit_niv3y$lower,upper=fit_niv3y$upper)

fit_niv5y<-survfit(Surv(ipi5y$recyrs,ipi5y$censrec)~1,data=ipi5y)
data_fit_niv5y<-data.frame(time=fit_niv5y$time, surv=fit_niv5y$surv,lower=fit_niv5y$lower,upper=fit_niv5y$upper)


source("3-further-output.R")






