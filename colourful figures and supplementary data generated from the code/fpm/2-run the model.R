
# further process data
flexmodel<-function(ltHaz,ltHaz_out,sd_bc,sd_bc_out,df_cure,pm_ind){
  follow_up <- 36 #origin study
  numMod <- 16 # Models considered
  MyTH <- 6.5 # Time Horizon (years) (origin mature study)
  MyStep <- 12 # Number of obs. per year
  MyN <- MyTH*MyStep # Total time points (observed & extrapolated)
  dfHazEst <- array(dim=c(numMod, MyN))
  Newtime <- data.frame(Time = ltHaz_out$Time, AtRisk = 1)
  Newtime$MyId <- 1:dim(Newtime)[1]
  Newtime$MyId <- ifelse(Newtime$MyId > follow_up, follow_up, Newtime$MyId)  # Random effects: Using last observed ID for extrapolation
  # Newtime$re<-scale(Newtime$MyId)
  Newtime$EventsL <- 0
  Newtime$EventsL[1:follow_up] <- lag(ltHaz$Events)
  Newtime$EventsL[1] <- 0
  Newtime$EventsL <- ifelse(Newtime$MyId > follow_up, 0, Newtime$EventsL) # AR: Using last observed event count for extrapolation
  Newtime$timedelta<-Newtime$Time[2]-Newtime$Time[1]
  Newtime$lnTime<-log(Newtime$Time)
  
  
  MyTH2 <- 20 # Time Horizon (years)
  MyStep2 <- 12 # Number of obs. per year
  MyN2 <- MyTH2*MyStep2 # Total time points (observed & extrapolated)
  dfHazEst2 <- array(dim=c(numMod, MyN2))
  dfHazEst3 <- array(dim=c(numMod, MyN2))
  Newtime2 <- data.frame(Time = c(ltHaz_out$Time,ltHaz_out$Time[78]+seq(Newtime$timedelta[1],Newtime$timedelta[1]*162,Newtime$timedelta[1])), AtRisk = 1)
  Newtime2$MyId <- 1:dim(Newtime2)[1]
  Newtime2$MyId <- ifelse(Newtime2$MyId > follow_up, follow_up, Newtime2$MyId)  # Random effects: Using last observed ID for extrapolation
  # Newtime2$re<-scale(Newtime2$MyId)
  Newtime2$EventsL <- 0
  Newtime2$EventsL[1:follow_up] <- lag(ltHaz$Events)
  Newtime2$EventsL[1] <- 0
  Newtime2$EventsL <- ifelse(Newtime2$MyId > follow_up, 0, Newtime2$EventsL) # AR: Using last observed event count for extrapolation
  Newtime2$timedelta<-Newtime2$Time[2]-Newtime2$Time[1]
  Newtime2$lnTime<-log(Newtime2$Time)
  # Also have 1x GOF matrix. Rows = Methods, Columns = Method, LL, AIC
  dfGOF <- data.frame(matrix(, nrow=16, ncol=5))
  dfGOF_out <- data.frame(matrix(, nrow=16, ncol=5))
  colnames(dfGOF) <- c("Model","LnL","Params","AIC","Additional inform")
  colnames(dfGOF_out) <- c("Model","LnL","Params","AIC","Additional inform")
  # Below is constant for when have to derive log-likelihood
  llCons <- sum(ltHaz$Events*log(ltHaz$AtRisk) - log(factorial(ltHaz$Events)))
  llCons_out<-sum(ltHaz_out$Events*log(ltHaz_out$AtRisk) - log(factorial(ltHaz_out$Events)))
  # Names of models to consider
  md<-c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","FP1","FP2","RCS","RP-hazard","RP-odds","RP-normal","GAM","param-mix","mix-cure")
  
  #Standard survival models(1-7)
  MODi <- 1 # Model index
  MyDists <- list("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma")
  for (i in 1:7){
    glmTemp <- flexsurvreg(Surv(recyrs, censrec) ~ 1, data = sd_bc, dist = MyDists[[i]])
    dfHazEst[MODi,] <- summary(glmTemp, t=Newtime$Time, type="hazard")[[1]]$est/12
    dfHazEst2[MODi,] <- summary(glmTemp, t=Newtime2$Time, type="hazard")[[1]]$est/12
    ltHaz[[MyDists[[i]]]] <- summary(glmTemp, t=ltHaz$Time, type="hazard")[[1]]$est/12
    dfGOF[MODi,1] <- MyDists[[i]]
    dfGOF[MODi,2] <- sum(ltHaz$Events*log(ltHaz[[MyDists[[i]]]]) - ltHaz[[MyDists[[i]]]]*ltHaz$AtRisk) + llCons
    dfGOF[MODi,3] <- glmTemp$npars
    coeff_temp<-as.data.frame(glmTemp$coefficients)
    t1<-"Param"
    for (i in 1:length(glmTemp$coefficients)) {
      t0<-paste(rownames(coeff_temp)[i],round(coeff_temp[i,1],2),sep = "=")
      t1<-paste(t1,t0,sep = ";")
    }
    dfGOF[MODi,5] <- t1
    MODi<-MODi+1
  }
  
  ##Fractional polynomial(8-9)
  #-----FP1 -----
  myLnL <- array(dim=8)
  myAIC <- array(dim=8)
  MyPowers <- list(c(-2,-1,-0.5,0.5,1,2,3))
  for (i in 1:7){
    glmTemp <- glm (cbind(Events,AtRisk-Events) ~ I(Time^MyPowers[[1]][i]) + offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)
    myLnL[i] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
    myAIC[i] <- extractAIC(glmTemp)[2]
  }
  ### run for 0
  glmTemp <- glm (cbind(Events,AtRisk-Events) ~ I(Time^0*lnTime) + offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)
  myLnL[8] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
  myAIC[8] <- extractAIC(glmTemp)[2]
  
  FP1res <- data.frame(c("-2","-1","-0.5","0","0.5","1","2","3"))
  FP1res <- cbind(FP1res,myLnL,myAIC)
  colnames(FP1res) <- c("Powers","LnL","AIC")
  FP1res <-arrange(FP1res,AIC)
  FP1_pow <- as.numeric(FP1res[1,1])
  
  #-----FP2 -----
  myLnL <- array(dim=36)
  myAIC <- array(dim=36)
  MyPowers <- list(c(-2,-1,-0.5,0.5,1,2,3))
  index <- 1
  for (i in 1:7){
    for (j in 1:7){
      if (j > i) {
        glmTemp <- glm(cbind(Events,AtRisk-Events) ~ I(Time^MyPowers[[1]][i]) + I(Time^MyPowers[[1]][j])+ offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)# 
        myLnL[index] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
        myAIC[index] <- extractAIC(glmTemp)[2]
        index <- index + 1
      }
    }
  }
  for (i in 1:7) {
    glmTemp <- glm(cbind(Events,AtRisk-Events) ~ I(Time^MyPowers[[1]][i]) + I(Time^MyPowers[[1]][i]*lnTime)+ offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)# 
    myLnL[index] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
    myAIC[index] <- extractAIC(glmTemp)[2]
    index <- index + 1
  }
  
  for (i in 1:7) {
    glmTemp <- glm(cbind(Events,AtRisk-Events) ~ I(Time^MyPowers[[1]][i]) + I(Time^0*lnTime)+ offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)# 
    myLnL[index] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
    myAIC[index] <- extractAIC(glmTemp)[2]
    index <- index + 1
  }
  
  glmTemp <- glm(cbind(Events,AtRisk-Events) ~ I(Time^0*lnTime) + I(Time^0*lnTime*lnTime)+ offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)# 
  myLnL[index] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
  myAIC[index] <- extractAIC(glmTemp)[2]
  
  FP2res <- data.frame(c("-2,-1","-2,-0.5","-2.0.5","-2,1","-2,2","-2,3","-1,-0.5","-1,0.5","-1,1","-1,2","-1,3","-0.5,0.5","-0.5,1","-0.5,2","-0.5,3",
                         "0.5,1","0.5,2","0.5,3","1,2","1,3","2,3","-2,-2","-1,-1","-0.5,-0.5","0.5,0.5","1,1","2,2","3,3",
                         "-2,0","-1,0","-0.5,0","0.5,0","1,0","2,0","3,0","0,0"))
  FP2res <- cbind(FP2res,myLnL,myAIC)
  colnames(FP2res) <- c("Powers","LnL","AIC")
  FP2res <-arrange(FP2res,AIC)
  FP2_pow_temp <- FP2res[1,1]
  t0<-as.data.frame(strsplit(FP2_pow_temp,","))
  FP2_pow_1<-as.numeric(t0[1,1])
  FP2_pow_2<-as.numeric(t0[2,1])
  
  ##summary results
  if(FP1_pow==0){
    modFP1 <- glm (cbind(Events,AtRisk-Events) ~ I(Time^0*lnTime) + offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)
  }else{modFP1 <- glm(cbind(Events,AtRisk-Events) ~ I(Time^FP1_pow)+ offset(log(timedelta)) , family=binomial(link=cloglog), data=ltHaz)}
  
  
  if(FP2_pow_1 == FP2_pow_2 & FP2_pow_2 != 0){
    modFP2<-glm(cbind(Events,AtRisk-Events) ~ I(Time^FP2_pow_1) + I(Time^FP2_pow_2*lnTime) + offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)
  }else if(FP2_pow_1==0 & FP2_pow_2==0){
    modFP2<-glm(cbind(Events,AtRisk-Events) ~ I(Time^0*lnTime) + I(Time^0*lnTime*lnTime)+ offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz) 
  }else if(FP2_pow_2==0 & FP2_pow_1 != 0){
    modFP2<-glm(cbind(Events,AtRisk-Events) ~ I(Time^MyPowers[[1]][i]) + I(Time^0*lnTime)+ offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)
  }else{
    modFP2<-glm(cbind(Events,AtRisk-Events) ~ I(Time^FP2_pow_1) + I(Time^FP2_pow_2) + offset(log(timedelta)), family=binomial(link=cloglog), data=ltHaz)
  }
  
  
  MODi <- 8
  dfGOF[MODi,1] <- md[MODi]
  dfGOF[MODi,2] <- (extractAIC(modFP1)[2] - 2*extractAIC(modFP1)[1])*(-0.5)
  dfGOF[MODi,3] <- extractAIC(modFP1)[1]
  dfGOF[MODi,5] <- paste("Param",FP1_pow,sep=";")
  
  # Hazard estimates
  dfHazEst[MODi,] <- predict(modFP1, newdata=Newtime, type="response") # Extrapolated
  dfHazEst2[MODi,] <- predict(modFP1, newdata=Newtime2, type="response") # Extrapolated
  ltHaz[md[MODi]] <- predict(modFP1, newdata=ltHaz, type="response")  # Within-sample
  
  
  MODi<-9
  dfGOF[MODi,1] <- md[MODi]
  dfGOF[MODi,2] <- (extractAIC(modFP2)[2] - 2*extractAIC(modFP2)[1])*(-0.5)
  dfGOF[MODi,3] <- extractAIC(modFP2)[1]
  dfGOF[MODi,5] <- paste("Param",FP2_pow_temp,sep=";")
  
  # Hazard estimates
  dfHazEst[MODi,] <- predict(modFP2, newdata=Newtime, type="response") # Extrapolated
  dfHazEst2[MODi,] <- predict(modFP2, newdata=Newtime2, type="response") # Extrapolated
  ltHaz[md[MODi]] <- predict(modFP2, newdata=ltHaz, type="response")  # Within-sample
  
  
  #Restricted cubic splines,10
  MODi <- 10
  # First need knot locations for up to 5 internal knots.
  # Basing these on equally-spaced percentiles of the observed (uncensored) death times.
  bc2 <- subset(niv3y, censrec==1)
  myLnL <- array(dim=5)
  myAIC <- array(dim=5)
  for (i in 1:5){
    glmTemp <- gam(cbind(Events,AtRisk-Events) ~ s(Time, bs="cr", k=i+2, fx=TRUE) + offset(log(timedelta)), knots=list(Time=quantile(bc2$recyrs, seq(from=0, to=1, by=1/(1+i))), length=i+2), family=binomial(link=cloglog), data=ltHaz)
    myLnL[i] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
    myAIC[i] <- extractAIC(glmTemp)[2]
  }
  RCSres <- data.frame(c("1","2","3","4","5"))
  RCSres <- cbind(RCSres,myLnL,myAIC)
  colnames(RCSres) <- c("Int.Knots","LnL","AIC")
  RCSres<-arrange(RCSres,AIC)
  RCS_k<-as.numeric(RCSres[1,1])
  i<-RCS_k
  glmTemp1 <- gam(cbind(Events,AtRisk-Events) ~ s(Time, bs="cr", k=i+2, fx=TRUE) + offset(log(timedelta)), knots=list(Time=quantile(bc2$recyrs, seq(from=0, to=1, by=1/(1+i))), length=i+2),family=binomial(link=cloglog), data=ltHaz)
  
  dfGOF[MODi,1] <- md[MODi]
  dfGOF[MODi,2] <- (extractAIC(glmTemp1)[2] - 2*extractAIC(glmTemp1)[1])*(-0.5)
  dfGOF[MODi,3] <- extractAIC(glmTemp1)[1]
  
  coeff_temp<-as.data.frame(glmTemp1$coefficients)
  t1<-"Param"
  for (i in 1:length(coeff_temp[,1])) {
    t0<-paste(rownames(coeff_temp)[i],round(coeff_temp[i,1],2),sep = "=")
    t1<-paste(t1,t0,sep = ";")
  }
  dfGOF[MODi,5] <- paste(t1,paste("k=",RCS_k),sep = ";")
  # Hazard estimates
  dfHazEst[MODi,] <- predict(glmTemp1, newdata=Newtime, type="response")
  dfHazEst2[MODi,] <- predict(glmTemp1, newdata=Newtime2, type="response")
  ltHaz[md[MODi]] <- predict(glmTemp1, newdata=ltHaz, type="response")
  
  #Royston-Parmar models,(11-13)
  MODi <- 11
  MyAIC <- array(dim=c(6,3))
  MyScale <- list("hazard","odds","normal")
  for (i in 1:3){
    for (j in 0:5){
      fit<-try(MyTemp <- flexsurvspline(Surv(recyrs, censrec) ~ 1, data = sd_bc_out, k = j, scale = MyScale[[i]]))
      if("try-error" %in% class(fit)) {
        MyAIC[[(i-1)*6+1+j]] <- "error"
      }
      else{
        flexsurvspline(Surv(recyrs, censrec) ~ 1, data = sd_bc_out, k = j, scale = MyScale[[i]])
        MyAIC[[(i-1)*6+1+j]] <- (-2*MyTemp$loglik+2*MyTemp$npars)
      }
    }
  }
  MyAICResults <- as.data.frame(cbind(seq(1:6)-1,MyAIC))
  colnames(MyAICResults) <- c("Int.Knots","Hazard","Odds","Normal")
  best_rp_hazard<-data.frame(MyAICResults$Int.Knots,MyAICResults$Hazard)
  colnames(best_rp_hazard)<-c("knots","AIC")
  best_rp_odds<-data.frame(MyAICResults$Int.Knots,MyAICResults$Odds)
  colnames(best_rp_odds)<-c("knots","AIC")
  best_rp_normal<-data.frame(MyAICResults$Int.Knots,MyAICResults$Normal)
  colnames(best_rp_normal)<-c("knots","AIC")
  best_rp_hazard<-arrange(best_rp_hazard,AIC)
  best_rp_odds<-arrange(best_rp_odds,AIC)
  best_rp_normal<-arrange(best_rp_normal,AIC)
  
  rp_input<-as.data.frame(array(dim=c(3,2)))
  rp_input$V1<- c("hazard","odds","normal")
  rp_input$V2<-c(best_rp_hazard[1,1],
                 best_rp_odds[1,1],
                 best_rp_normal[1,1])
  colnames(rp_input)<-c("scale","knots")
  rp_input$knots<-as.numeric(rp_input$knots)
  
  for (i in 1:3) {
    rp_scale<-rp_input[i,1]
    rp_k<-rp_input[i,2]
    rpTemp <- flexsurvspline(Surv(recyrs, censrec) ~ 1, data = sd_bc, k = rp_k, scale = rp_scale)
    rpAIC <- (-2*rpTemp$loglik+2*rpTemp$npars)
    dfHazEst[MODi,] <- summary(rpTemp, t=Newtime$Time, type="hazard")[[1]]$est/12
    dfHazEst2[MODi,] <- summary(rpTemp, t=Newtime2$Time, type="hazard")[[1]]$est/12
    ltHaz[md[MODi]] <- summary(rpTemp, t=ltHaz$Time, type="hazard")[[1]]$est/12
    
    dfGOF[MODi,1] <- md[MODi]
    dfGOF[MODi,2] <- sum(ltHaz$Events*log(ltHaz[md[MODi]]) - ltHaz[[md[MODi]]]*ltHaz$AtRisk) + llCons
    dfGOF[MODi,3] <- rpTemp$npars  
    
    coeff_temp<-as.data.frame(rpTemp$coefficients)
    t1<-"Param"
    for (i in 1:length(coeff_temp[,1])) {
      t0<-paste(rownames(coeff_temp)[i],round(coeff_temp[i,1],2),sep = "=")
      t1<-paste(t1,t0,sep = ";")
    }
    dfGOF[MODi,5] <- paste(t1,paste("knot=",rp_k),paste("scale=",rp_scale),sep = ";")
    MODi<-MODi+1
  } 
  
  #Generalised additive models
  MODi <- 14
  # Now base knots on minimising AIC
  myLnL <- array(dim=10)
  myAIC <- array(dim=10)
  for (i in 1:10){
    glmTemp <- gam(cbind(Events,AtRisk-Events) ~ s(Time, bs="cr", k=i+2, fx=FALSE) + offset(log(timedelta)), knots=list(Time=quantile(bc2$recyrs, seq(from=0, to=1, by=1/(1+i))), length=i+2), family=binomial(link=cloglog), data=ltHaz)
    myLnL[i] <- (extractAIC(glmTemp)[2] - 2*extractAIC(glmTemp)[1])*(-0.5)
    myAIC[i] <- extractAIC(glmTemp)[2]
  }
  GAMres <- data.frame(c("1","2","3","4","5"))
  GAMres <- cbind(GAMres,myLnL,myAIC)
  colnames(GAMres) <- c("Int.Knots","LnL","AIC")
  GAMres<-arrange(GAMres,AIC)
  GAM_k<-as.numeric(GAMres[1,1])
  i<-GAM_k
  glmTemp1 <- gam(cbind(Events,AtRisk-Events) ~ s(Time, bs="cr", k=i+2, fx=FALSE) + offset(log(timedelta)), knots=list(Time=quantile(bc2$recyrs, seq(from=0, to=1, by=1/(1+i))), length=i+2),family=binomial(link=cloglog), data=ltHaz)
  
  dfGOF[MODi,1] <- md[MODi]
  dfGOF[MODi,2] <- (extractAIC(glmTemp1)[2] - 2*extractAIC(glmTemp1)[1])*(-0.5)
  dfGOF[MODi,3] <- extractAIC(glmTemp1)[1]
  coeff_temp<-as.data.frame(glmTemp1$coefficients)
  t1<-"Param"
  for (i in 1:length(coeff_temp[,1])) {
    t0<-paste(rownames(coeff_temp)[i],round(coeff_temp[i,1],2),sep = "=")
    t1<-paste(t1,t0,sep = ";")
  }
  smooth_parm<-as.data.frame(glmTemp1$sp)
  t2<-"Smooth_Param"
  for (i in 1:length(smooth_parm[,1])) {
    t0<-paste(rownames(smooth_parm)[i],round(smooth_parm[i,1],2),sep = "=")
    t2<-paste(t2,t0,sep = ";")
  }
  dfGOF[MODi,5] <- paste(paste(t1,paste("k=",GAM_k),sep = ";"),t2,sep = ";")
  # Hazard estimates
  dfHazEst[MODi,] <- predict(glmTemp1, newdata=Newtime, type="response")
  dfHazEst2[MODi,] <- predict(glmTemp1, newdata=Newtime2, type="response")
  ltHaz[md[MODi]] <- predict(glmTemp1, newdata=ltHaz, type="response")
  
  #paramter-mixture models,15
  #  Code Reference:  #
  #-----------------------------------------------------------#
  # Authors and copyright:                                    #
  # Sven Klijn, Marjanne Piena, Sonja Kroep, Craig Bennison   #
  # Pharmerit - an OPEN Health company                        #
  # November 2020                                             #
  #-----------------------------------------------------------#
  ##text2
  flexsurvmixture <- function(formula, dists, control, sr.control){
    
    n.groups <- length(dists)
    
    dist_list                 <- list()
    dist_list$name            <- paste0(n.groups,"groups: ",paste0(dists,"_", collapse=""))
    dist_list$pars            <- do.call(compile.pars, args = list(dists, n.groups))
    dist_list$location        <- "p1"
    dist_list$transforms      <- do.call(compile.transform, args = list(dists, n.groups))
    dist_list$inv.transforms  <- do.call(compile.inv.transform, args = list(dists, n.groups))
    dist_list$inits           <- function(t){do.call(compile.inits, args = list(t = t, dists, n.groups))}
    
    pfun <- dfun <- list()
    for(i in 1:n.groups){
      pfun[[i]] = get(paste0("p", dists[i]))
      dfun[[i]] = get(paste0("d", dists[i]))
    }
    
    dfns_list = list(
      p = function(q, ...) pmixsurv(pfun, q, n.groups, ...),
      d = function(x, ...) dmixsurv(dfun, x, n.groups, ...),
      mean = function(...) mean_mixsurv(pfun, ...),
      rmst = function(t, ...) rmst_mixsurv(pfun, t, ...) 
    )
    
    optim <- list()
    out <- do.call(
      "flexsurvreg",
      append(
        list(
          formula,
          dist = dist_list,
          dfns = dfns_list,
          control = control, 
          sr.control = sr.control
        ),
        optim
      )
    )
    return(out)
  }
  pmixsurv = function(pfun, q, n.groups, ...) {
    dots <- list(...)
    args <- dots
    args$lower.tail <- F
    args$log.p <- F
    
    theta                     <- c()
    theta[1]                  <- args$p1; args$p1 <- NULL
    if(n.groups > 2){theta[2] <- args$p2; args$p2 <- NULL}
    if(n.groups > 3){theta[3] <- args$p3; args$p3 <- NULL}
    theta <- multinom.p(theta)
    
    args1 <- args; args1[(endsWith(names(args1), "2") | endsWith(names(args1), "3") | endsWith(names(args1), "4"))] <- NULL
    args2 <- args; args2[(endsWith(names(args2), "1") | endsWith(names(args2), "3") | endsWith(names(args2), "4"))] <- NULL
    args3 <- args; args3[(endsWith(names(args3), "1") | endsWith(names(args3), "2") | endsWith(names(args3), "4"))] <- NULL
    args4 <- args; args4[(endsWith(names(args4), "1") | endsWith(names(args4), "2") | endsWith(names(args4), "3"))] <- NULL
    
    names(args1)[endsWith(names(args1), "1")] <- gsub("1", "", names(args1)[endsWith(names(args1), "1")])
    names(args2)[endsWith(names(args2), "2")] <- gsub("2", "", names(args2)[endsWith(names(args2), "2")])
    names(args3)[endsWith(names(args3), "3")] <- gsub("3", "", names(args3)[endsWith(names(args3), "3")])
    names(args4)[endsWith(names(args4), "4")] <- gsub("4", "", names(args4)[endsWith(names(args4), "4")])
    
    out <- (theta[1] * do.call(pfun[[1]], append(list(q), args1)) + 
              theta[2] * do.call(pfun[[2]], append(list(q), args2)) +
              ifelse(n.groups>2, theta[3] * do.call(pfun[[3]], append(list(q), args3)),0) +
              ifelse(n.groups>3, theta[4] * do.call(pfun[[4]], append(list(q), args4)),0)) 
    
    if (is.null(dots$lower.tail) || dots$lower.tail) {
      out <- 1 - out
    }
    if (!is.null(dots$log.p) && dots$log.p) {
      out <- log(out)
    }
    return(out)
  }
  dmixsurv = function(dfun, x, n.groups, ...) {
    dots <- list(...)
    args <- dots
    args$log <- F
    
    theta                     <- c()
    theta[1]                  <- args$p1; args$p1 <- NULL
    if(n.groups > 2){theta[2] <- args$p2; args$p2 <- NULL}
    if(n.groups > 3){theta[3] <- args$p3; args$p3 <- NULL}
    theta <- multinom.p(theta)
    
    args1 <- args; args1[(endsWith(names(args1), "2") | endsWith(names(args1), "3") | endsWith(names(args1), "4"))] <- NULL
    args2 <- args; args2[(endsWith(names(args2), "1") | endsWith(names(args2), "3") | endsWith(names(args2), "4"))] <- NULL
    args3 <- args; args3[(endsWith(names(args3), "1") | endsWith(names(args3), "2") | endsWith(names(args3), "4"))] <- NULL
    args4 <- args; args4[(endsWith(names(args4), "1") | endsWith(names(args4), "2") | endsWith(names(args4), "3"))] <- NULL
    
    names(args1)[endsWith(names(args1), "1")] <- gsub("1", "", names(args1)[endsWith(names(args1), "1")])
    names(args2)[endsWith(names(args2), "2")] <- gsub("2", "", names(args2)[endsWith(names(args2), "2")])
    names(args3)[endsWith(names(args3), "3")] <- gsub("3", "", names(args3)[endsWith(names(args3), "3")])
    names(args4)[endsWith(names(args4), "4")] <- gsub("4", "", names(args4)[endsWith(names(args4), "4")])
    
    
    out <- (theta[1] * do.call(dfun[[1]], append(list(x), args1)) + 
              theta[2] * do.call(dfun[[2]], append(list(x), args2)) +  
              ifelse(n.groups>2,theta[3] * do.call(dfun[[3]], append(list(x), args3)),0) + 
              ifelse(n.groups>3,theta[4] * do.call(dfun[[4]], append(list(x), args4)),0))
    
    if (!is.null(dots$log) && dots$log) {
      out <- log(out)
    }
    return(out)
  }
  rmst_mixsurv = function(pfun, q, t, ...) {
    args <- list(...)
    out <- do.call(
      rmst_generic,
      append(
        list(
          function(q, ...) pmixsurv(pfun, q, ...),
          t = t
        ),
        args
      )
    )
    return(out)
  }
  mean_mixsurv = function(pfun, ...) {
    if(p1 > 0) {
      out <- Inf
    }else {
      args <- list(...)
      out <- do.call(
        rmst_generic,
        append(
          list(
            pfun[[1]],
            t = Inf,
            start = 0
          ),
          args
        )
      )
    }
    return(out)
  }
  multinom.p = function(theta) {
    p <- rep(NA,length(theta) + 1)
    for (i in 1:length(theta)) {
      p[i] <- exp(theta[i])/(1 + sum(exp(theta)))
    }
    p[length(theta) + 1] <- 1 / (1 + sum(exp(theta)))
    return(p)
  }
  #exp
  exp.inits <- function(t) {
    1/mean(t)
  }
  exp.pars <- flexsurv.dists$exp$pars
  exp.t <- list(log)
  exp.it <- list(exp)
  #gamma
  gamma.inits <- function(t) {
    m = mean(t)
    v = var(t)
    c(m^2/v, m/v)
  }
  gamma.pars <- flexsurv.dists$gamma$pars
  gamma.t <- list(log, log)
  gamma.it <- list(exp, exp)
  #gen gamma
  gengamma.inits <- function(t) {
    lt <- log(t[t > 0])
    c(mean(lt), sd(lt), 0)
  }
  gengamma.pars <- flexsurv.dists$gengamma$pars
  gengamma.t <- list(identity, log, identity)
  gengamma.it <- list(identity, exp, identity)
  #gompertz
  gompertz.inits <- function(t) {
    c(0.001, 1/mean(t))
  }
  gompertz.pars <- flexsurv.dists$gompertz$pars
  gompertz.t <- list(identity, log)
  gompertz.it <- list(identity, exp)
  #llogis
  llogis.inits <- function(t) {
    scale <- median(t)
    shape <- 1/log(quantile(t, 0.25)/scale, base = 3)
    if (shape < 0) 
      shape <- 1
    c(shape, scale)
  }
  llogis.pars <- flexsurv.dists$llogis$pars
  llogis.t <- list(log, log)
  llogis.it <- list(exp, exp)
  #lnorm
  lnorm.inits <- function(t) {
    lt <- log(t[t > 0])
    c(mean(lt), sd(lt))
  }
  lnorm.pars <- flexsurv.dists$lnorm$pars
  lnorm.t <- list(identity, log)
  lnorm.it <- list(identity, exp)
  #weibull
  weibull.inits <- function(t) {
    lt <- log(t[t > 0])
    c(1.2/var(lt), exp(mean(lt) + 0.572)) 
  }
  weibull.pars <- flexsurv.dists$weibull$pars
  weibull.t <- list(log, log)
  weibull.it <- list(exp, exp)
  #compile
  compile.inits <- function(t, group.dists, n.groups) {
    if(n.groups >= 1) inits.groups <- rep(0.01, n.groups - 1)
    inits1 <- get(paste0(group.dists[1], ".inits"))
    if(!is.na(group.dists[2])) inits2 <- get(paste0(group.dists[2], ".inits")) else inits2 <- function(t){NULL}
    if(!is.na(group.dists[3])) inits3 <- get(paste0(group.dists[3], ".inits")) else inits3 <- function(t){NULL}
    if(!is.na(group.dists[4])) inits4 <- get(paste0(group.dists[4], ".inits")) else inits4 <- function(t){NULL}
    if(n.groups == 1) return(inits1(t))
    if(n.groups == 2) return(c(inits.groups, inits1(t), inits2(t)))
    if(n.groups == 3) return(c(inits.groups, inits1(t), inits2(t), inits3(t)))
    if(n.groups == 4) return(c(inits.groups, inits1(t), inits2(t), inits3(t), inits4(t)))
    if(n.groups >= 4) stop("too many distributions specified, use 4 groups or fewer")
  }
  compile.pars <- function(group.dists, n.groups) {
    pars.groups = NULL
    if(n.groups >= 1) for(i in 1:(n.groups-1)) pars.groups[i] <- paste0("p",i)
    pars1 <- paste0(get(paste0(group.dists[1], ".pars")),1)
    if(!is.na(group.dists[2])) pars2 <- paste0(get(paste0(group.dists[2], ".pars")),2) else pars2 <- function(){NULL}
    if(!is.na(group.dists[3])) pars3 <- paste0(get(paste0(group.dists[3], ".pars")),3) else pars3 <- function(){NULL}
    if(!is.na(group.dists[4])) pars4 <- paste0(get(paste0(group.dists[4], ".pars")),4) else pars4 <- function(){NULL}
    if(n.groups == 1) return(pars1)
    if(n.groups == 2) return(c(pars.groups, pars1, pars2))
    if(n.groups == 3) return(c(pars.groups, pars1, pars2, pars3))
    if(n.groups == 4) return(c(pars.groups, pars1, pars2, pars3, pars4))
    if(n.groups >= 4) stop("too many distributions specified, use 4 groups or fewer")
  }
  compile.transform <- function(group.dists, n.groups) {
    tgroups = list(NULL)
    if(n.groups >= 1) for(i in 1:(n.groups-1)) tgroups[[i]] <- identity
    t1 <- get(paste0(group.dists[1], ".t"))
    if(!is.na(group.dists[2])) t2 <- get(paste0(group.dists[2], ".t")) else t2 <- function(){NULL}
    if(!is.na(group.dists[3])) t3 <- get(paste0(group.dists[3], ".t")) else t3 <- function(){NULL}
    if(!is.na(group.dists[4])) t4 <- get(paste0(group.dists[4], ".t")) else t4 <- function(){NULL}
    if(n.groups == 1) return(t1)
    if(n.groups == 2) return(c(tgroups, t1, t2))
    if(n.groups == 3) return(c(tgroups, t1, t2, t3))
    if(n.groups == 4) return(c(tgroups, t1, t2, t3, t4))
    if(n.groups >= 4) stop("too many distributions specified, use 4 groups or fewer")
  }
  compile.inv.transform <- function(group.dists, n.groups) {
    itgroups = list(NULL)
    if(n.groups >= 1) for(i in 1:(n.groups-1)) itgroups[[i]] <- identity
    it1 <- get(paste0(group.dists[1], ".it"))
    if(!is.na(group.dists[2])) it2 <- get(paste0(group.dists[2], ".it")) else it2 <- function(){NULL}
    if(!is.na(group.dists[3])) it3 <- get(paste0(group.dists[3], ".it")) else it3 <- function(){NULL}
    if(!is.na(group.dists[4])) it4 <- get(paste0(group.dists[4], ".it")) else it4 <- function(){NULL}
    if(n.groups == 1) return(it1)
    if(n.groups == 2) return(c(itgroups, it1, it2))
    if(n.groups == 3) return(c(itgroups, it1, it2, it3))
    if(n.groups == 4) return(c(itgroups, it1, it2, it3, it4))
    if(n.groups >= 4) stop("too many distributions specified, use 4 groups or fewer")
  }
  
  ## simulate results ##
  #newdata
  data_pm<-sd_bc
  data_pm$rectime2<-as.numeric(data_pm$recyrs*12) 
  data_pm$recyrs<-as.numeric(data_pm$recyrs)
  survgef <- Surv(time=data_pm$rectime2 , event=data_pm$censrec==1)
  # plot(survgef)
  
  # dists list #
  dists_MM<-as.data.frame(matrix(nrow=28,ncol=2))
  colnames(dists_MM)<-c("dist1",'dist2')
  distlist<-c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma")
  
  index1<-1
  for (i in 1:7) {
    for (j in i:7){
      dists_MM$dist1[index1]<- distlist[i]
      dists_MM$dist2[index1]<- distlist[j]
      index1<-index1+1
    }
  }
  
  myAIC <- array(dim=28)
  for (i in 1:28) {
    mixfit <- flexsurvmixture(survgef ~ 1, dists = c(dists_MM$dist1[i],dists_MM$dist2[i]), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
    myAIC[i] <- (-2*mixfit$loglik+2*mixfit$npars)
  }
  pm_temp_aic<-as.data.frame(cbind(seq(1,28,1),myAIC))
  pm_temp_aic<-arrange(pm_temp_aic,myAIC)
  param1<-dists_MM[pm_temp_aic$V1[1],]
  param2<-dists_MM[pm_temp_aic$V1[2],]
  
  
  # best mixture model
  if(pm_ind==1){
    distMM_best<-data.frame(dist1=param1[1],dist2=param1[2])
  }else if(pm_ind==2){
    distMM_best<-data.frame(dist1=param2[1],dist2=param2[2])
  }
  
  mixfit <- flexsurvmixture(survgef ~ 1, dists = c(distMM_best$dist1,distMM_best$dist2), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
  
  #mixfit <- flexsurvmixture(survgef ~ 1, dists = c("llogis","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
  # Plot mixture model
  # name_temp<-paste(plotname1,"_MM_test.png",sep = "")
  # png( 
  #   filename = name_temp, # 文件名称
  #   width = 600,           # 宽
  #   height = 480,          # 高
  #   units = "px",          # 单位
  #   bg = "white",          # 背景颜色
  #   res = 120)              # 分辨率
  # ## KM curve
  # plot(survgef)
  # ## Component 1: weibull
  # lines(seq.int(max(48)), 1-pllogis(seq.int(48), mixfit$res[2],mixfit$res[3]), col = "blue", lwd = 1) 
  # ## Component 2: gengamma
  # lines(seq.int(48), 1-pgengamma(seq.int(48), mixfit$res[4], mixfit$res[5], mixfit$res[6]), col = "cyan", lwd = 1)
  # ## Mixture model
  # lines(summary(mixfit, t = seq.int(48))[[1]][,c("time", "est")], col = "red", lwd = 2)
  # #
  # # dev.off()
  # # write data
  MODi<-15
  
  dfHazEst[MODi,]<-summary(mixfit, t = Newtime$Time*12,type="hazard")[[1]]$est
  dfHazEst2[MODi,]<-summary(mixfit, t = Newtime2$Time*12,type="hazard")[[1]]$est
  ltHaz[md[MODi]] <- summary(mixfit, t=ltHaz$Time*12,type="hazard")[[1]]$est
  surv_MM_lthaz<-summary(mixfit, t=ltHaz$Time*12,type="survival")[[1]]$est
  surv_MM_df2<-summary(mixfit, t = Newtime2$Time*12,type="survival")[[1]]$est
  
  dfGOF[MODi,1] <- md[MODi]
  dfGOF[MODi,2] <- sum(ltHaz$Events*log(ltHaz[md[MODi]]) - ltHaz[md[MODi]]*ltHaz$AtRisk) + llCons
  dfGOF[MODi,3] <- mixfit$npars 
  coeff_temp<-as.data.frame(mixfit$coefficients)
  t1<-"Param"
  for (i in 1:length(mixfit$coefficients)) {
    t0<-paste(rownames(coeff_temp)[i],round(coeff_temp[i,1],2),sep = "=")
    t1<-paste(t1,t0,sep = ";")
  }
  dfGOF[MODi,5] <- paste(paste(distMM_best$dist1,distMM_best$dist2,sep = "-"),t1,sep = ":") 
  
  
  
  
  #mix cure models,16
  #load data
  ## general cure rate
  # Function to convert between rates and probability
  haz_rate = function(x, t, out = "prob"){ 
    tmp  = t - lag(t, default = 0)
    if (out == "rate"){
      y = case_when(x == 1 ~ 1, TRUE ~ - (log(1 - x)) / tmp)
    } else if (out == "prob") {
      y = 1 - exp(- x * tmp)
    } else {
      "error!"
    }
    return (y)
  }
  # England 2016  # should be changed
  # Eng_HMD = read.table(here("UK_HMDv2.txt"), header=TRUE)
  # Eng_2016 = filter(Eng_HMD, Year == 2016) %>% mutate(tmp = Age,
  #                                                     Age = as.numeric(tmp)[tmp],
  #                                                     Hazard = haz_rate(qx, 1, "rate"),  # All observations 1 unit apart
  #                                                     Surv = (lx - dx) / lx[1],
  #                                                     Years = Age - 62)
  # tmp = select(Eng_2016, Year, Age, Hazard)
  Eng_HMD = read.table(here("USA_LF.txt"), header=TRUE)
  USA_HMD = as.data.frame(matrix(ncol = 5,nrow = 84))
  colnames(USA_HMD)<-c("Age","qx","lx","dx","Year")
  USA_HMD$Age<-seq(1,84,1)
  for (i in 1:4) {
    USA_HMD$qx[i]<-Eng_HMD$qx[1]
    USA_HMD$lx[i]<-Eng_HMD$lx[1]
    USA_HMD$dx[i]<-Eng_HMD$dx[1]
  }
  for (i in 2:17) {
    for (j in ((i-1)*5):(i*5-1)) {
      USA_HMD$qx[j]<-Eng_HMD$qx[i]
      USA_HMD$lx[j]<-Eng_HMD$lx[i]
      USA_HMD$dx[j]<-Eng_HMD$dx[i]  
    }
  }
  USA_HMD$Year<-2015
  
  USA_2015<-as.data.frame(matrix(ncol = 4,nrow = 84))
  colnames(USA_2015)<-c("Age","Hazard","Surv","Years")
  USA_2015$Age<-USA_HMD$Age
  USA_2015$Hazard[1:4]<-haz_rate(USA_HMD$qx[1:4], 4, "rate")
  USA_2015$Hazard[5:84]<-haz_rate(USA_HMD$qx[5:84], 5, "rate")
  USA_2015$Surv<-(USA_HMD$lx-USA_HMD$dx)/USA_HMD$lx
  USA_2015$Years<-USA_2015$Age-62
  Eng_2016<-cbind(USA_HMD,USA_2015[,2:4])
  tmp = select(Eng_2016, Year, Age, Hazard)
  
  
  # general information
  my_time<-ltHaz$Time
  my_time2<-Newtime$Time
  my_time3<-Newtime2$Time
  
  df_GP1 = tibble(Time = my_time, 
                  Haz_pop = approx(x = Eng_2016$Years, y = Eng_2016$Hazard, xout = my_time, method = "constant")$y,
                  Srv_pop = approx(x = Eng_2016$Years, y = Eng_2016$Surv, xout = my_time, method = "constant")$y)
  df_GP2 = tibble(Time = my_time2, 
                  Haz_pop = approx(x = Eng_2016$Years, y = Eng_2016$Hazard, xout = my_time2, method = "constant")$y,
                  Srv_pop = approx(x = Eng_2016$Years, y = Eng_2016$Surv, xout = my_time2, method = "constant")$y)
  df_GP3 = tibble(Time = my_time3, 
                  Haz_pop = approx(x = Eng_2016$Years, y = Eng_2016$Hazard, xout = my_time3, method = "constant")$y,
                  Srv_pop = approx(x = Eng_2016$Years, y = Eng_2016$Surv, xout = my_time3, method = "constant")$y)
  # ipd data
  #my_time = seq(from=0.25, to=7.5, by=0.05) # Time values for which we want estimates.
  my_df_ipd<-df_cure
  my_df_ipd=my_df_ipd %>% mutate(Year = 2015, Age = floor(62 + Obs_surv))
  my_df_ipd<-left_join(my_df_ipd, tmp, by = c("Year", "Age"))
  
  ###-----------------------------------------------------######-----------------------------------------------------######-----------------------------------------------------###
  ###-----------------------------------------------------######-----------------------------------------------------######-----------------------------------------------------###
  myLnL <- array(dim=3)
  myPram<-array(dim=3)
  myAIC <- array(dim=3)
  myPS<-array(dim=3)
  
  ###-----------------------------------------------------###
  ###               mixture  cure   model
  ###-----------------------------------------------------###
  ### weibull CuRe
  df <- my_df_ipd
  best_wei <- fit.cure.model(Surv(Obs_surv, Censor) ~ 1, data = df, bhazard = "Hazard",formula.surv = list(~ 1, ~ 1), dist = "weibull")
  myLnL[1] <- best_wei$ML
  myPram[1]<- length(best_wei$coefs)
  myAIC[1] <- 2*(length(best_wei$coefs) + -best_wei$ML)
  cure_p1 <- predict(best_wei, type = "curerate")[[1]]$Estimate
  #   Save data on hazard and survival for those with disease (uncured)
  df_best_wei <- tibble(Time = my_time, Srv_dis = predict(best_wei, type = "survuncured", time = my_time)[[1]]$Estimate,
                        Haz_dis = predict(best_wei, type = "hazarduncured", time = my_time)[[1]]$Estimate)
  t_cw<-"Param"
  for (i in 1:length(best_wei$coefs)) {
    t0<-paste(paste("coefs",i,sep = "-"),round(best_wei$coefs[[i]],4),sep = "=")
    t_cw<-paste(t_cw,t0,sep = ";")
  }
  t_cw<-paste(t_cw,paste("cure_p",round(cure_p1,4),sep = ":"),sep = ";")
  myPS[1]<-t_cw
  ### Lnorm CuRe
  df <- my_df_ipd
  best_lnorm <- fit.cure.model(Surv(Obs_surv, Censor) ~ 1, data = df, bhazard = "Hazard",formula.surv = list(~ 1, ~ 1), dist = "lognormal")
  myLnL[2] <- best_lnorm$ML
  myPram[2]<- length(best_lnorm$coefs)
  myAIC[2] <- 2*(length(best_lnorm$coefs) + -best_lnorm$ML)
  cure_p2 <- predict(best_lnorm, type = "curerate")[[1]]$Estimate
  #   Save data on hazard and survival for those with disease (uncured)
  df_best_lnorm <- tibble(Time = my_time, Srv_dis = predict(best_lnorm, type = "survuncured", time = my_time)[[1]]$Estimate,
                          Haz_dis = predict(best_lnorm, type = "hazarduncured", time = my_time)[[1]]$Estimate)
  t_cl<-"Param"
  for (i in 1:length(best_lnorm$coefs)) {
    t0<-paste(paste("coefs",i,sep = "-"),round(best_lnorm$coefs[[i]],4),sep = "=")
    t_cl<-paste(t_cl,t0,sep = ";")
  }
  t_cl<-paste(t_cl,paste("cure_p",round(cure_p2,4),sep = ":"),sep = ";")
  myPS[2]<-t_cl
  
  ### RP CuRe
  max_k<-5
  df <- my_df_ipd
  my_k = 1
  myAICrp <- array(dim=5)
  for(i in 1:max_k){
    tmp = GenFlexCureModel(Surv(Obs_surv, Censor) ~ 1, data = df, bhazard = "Hazard", df= i, verbose = FALSE)
    tmp_AIC = 2*(length(tmp$coefs) + length(tmp$coefs.spline) + tmp$NegMaxLik)
    myAICrp[i]<-tmp_AIC
  }
  kmin<-which.min(myAICrp)
  
  best_RP<-GenFlexCureModel(Surv(Obs_surv, Censor) ~ 1, data = df, bhazard = "Hazard", df= kmin, verbose = FALSE)
  myAIC[3]<-2*(length(best_RP$coefs) + length(best_RP$coefs.spline) + best_RP$NegMaxLik)
  myLnL[3]<--best_RP$NegMaxLik
  myPram[3]<-length(best_RP$coefs) + length(best_RP$coefs.spline)
  cure_p_mc <- predict(best_RP, type = "curerate")[[1]]$Estimate
  # Save data on hazard and survival for those with disease (uncured)
  df_RPcure = tibble(Time = my_time, Srv_dis = predict(best_RP, type = "survuncured", time = my_time)[[1]]$Estimate,
                     Haz_dis = predict(best_RP, type = "hazarduncured", time = my_time)[[1]]$Estimate)
  t_RP<-"Param"
  for (i in 1:length(best_RP$coefs)) {
    t0<-paste(paste("coefs",i,sep = "-"),round(best_RP$coefs[i],4),sep = "=")
    t_RP<-paste(t_RP,t0,sep = ";")
  }
  for (i in 1:length(best_RP$coefs.spline)) {
    t0<-paste(paste("coefs-spline",i,sep = "-"),round(best_RP$coefs.spline[i],4),sep = "=")
    t_RP<-paste(t_RP,t0,sep = ";")
  }
  t_RP<-paste(t_RP,paste("cure_p",round(cure_p_mc,4),sep = ":"),sep = ";")
  myPS[3]<-t_RP
  
  
  cm_name<-c("mix-wei","mix-lnorm","mix-RP")
  cm_res<-cbind(cm_name,myLnL,myPram,myAIC,myPS)
  cm_res<-as.data.frame(cm_res)
  cm_res<-arrange(cm_res,myAIC)
  
  cm_res[1,1] 
  
  if (cm_res[1,1] == "mix-RP"){
    dfmod_cure<-best_RP
    cure_p<-cure_p_mc
  } else if(cm_res[1,1] == "mix-lnorm"){
    dfmod_cure<-best_lnorm
    cure_p<-cure_p2
  } else {
    dfmod_cure<-best_wei
    cure_p<-cure_p1
  }
  
  MODi<-16
  #lthaz
  df1<-tibble(Time = my_time, Srv_dis = predict(dfmod_cure, type = "survuncured", time = my_time)[[1]]$Estimate,
              Haz_dis = predict(dfmod_cure, type = "hazarduncured", time = my_time)[[1]]$Estimate)
  df_mod_1 <- left_join(df1, df_GP1, by = "Time") %>% mutate(Srv_mod = Srv_pop * cure_p + Srv_dis * (1 - cure_p),
                                                             Pred = Haz_pop + ((1-cure_p)*Haz_dis * Srv_dis)/(cure_p+(1-cure_p)*Srv_dis))
  
  ##  dfhazest1  ##
  df2 = tibble(Time = my_time2, Srv_dis = predict(dfmod_cure, type = "survuncured", time = my_time2)[[1]]$Estimate,
               Haz_dis = predict(dfmod_cure, type = "hazarduncured", time = my_time2)[[1]]$Estimate)
  df_mod_2 <- left_join(df2, df_GP2, by = "Time") %>% mutate(Srv_mod = Srv_pop * cure_p + Srv_dis * (1 - cure_p),
                                                             Pred = Haz_pop + ((1-cure_p)*Haz_dis * Srv_dis)/(cure_p+(1-cure_p)*Srv_dis))
  
  ##  dfhazest2  ##
  df3 = tibble(Time = my_time3, Srv_dis = predict(dfmod_cure, type = "survuncured", time = my_time3)[[1]]$Estimate,
               Haz_dis = predict(dfmod_cure, type = "hazarduncured", time = my_time3)[[1]]$Estimate)
  df_mod_3 <- left_join(df3, df_GP3, by = "Time") %>% mutate(Srv_mod = Srv_pop * cure_p + Srv_dis * (1 - cure_p),
                                                             Pred = Haz_pop + ((1-cure_p)*Haz_dis * Srv_dis)/(cure_p+(1-cure_p)*Srv_dis))
  
  dfHazEst[MODi,]<-df_mod_2$Pred/12
  dfHazEst2[MODi,]<-df_mod_3$Pred/12
  ltHaz[md[MODi]] <- df_mod_1$Pred/12
  
  mixcure_surv_mod1<-df_mod_1$Srv_mod
  mixcure_surv_mod2<-df_mod_2$Srv_mod
  mixcure_surv_mod3<-df_mod_3$Srv_mod
  
  dfGOF[MODi,1] <- md[MODi]
  dfGOF[MODi,2] <- cm_res$myLnL[1]
  dfGOF[MODi,3] <- cm_res$myPram[1]
  dfGOF[MODi,5] <- paste(cm_res$cm_name[1],cm_res$myPS[1],sep = ":")
  
  ###dfsurv process
  
  dfHaz <- t(dfHazEst2)
  colnames(dfHaz) <- c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","FP1","FP2","RCS","RP-hazard","RP-odds","RP-normal","GAM","param-mix","mix-cure")
  dfHaz <- cbind(data.frame(Newtime2$Time), dfHaz)
  #
  dfSurv<-as.data.frame(matrix(nrow = 240,ncol = 17))
  dfSurv[,1]<-dfHaz$Newtime2.Time
  for ( i in 2:15 ) {
    dftemp<-data.frame(dfHaz$Newtime2.Time,dfHaz[[i]])
    dftemp<-dftemp %>% 
      dplyr::arrange(dftemp[[1]]) %>% 
      dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>% 
      dplyr::mutate(survProp = exp(-1*cumhaz))
    dfSurv[[i]]<-dftemp[[4]]
  }
  dfSurv$V16<-surv_MM_df2
  dfSurv$V17<-mixcure_surv_mod3
  prime1<-c(0,rep(1,16))
  dfSurv<-rbind(prime1,dfSurv)
  colnames(dfSurv) <-c("Time",md)
  
  dfHaz2<-ltHaz[,11:26]
  colnames(dfHaz2) <- c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","FP1","FP2","RCS","RP-hazard","RP-odds","RP-normal","GAM","param-mix","mix-cure")
  #,"mix-cure","non-mix-cure"
  dfHaz2 <- cbind(data.frame(ltHaz$Time), dfHaz2)
  dfSurv2<-as.data.frame(matrix(nrow = 36,ncol = 17))
  dfSurv2[,1]<-dfHaz2$ltHaz.Time
  for ( i in 2:15 ) {
    dftemp<-data.frame(dfHaz2$ltHaz.Time,dfHaz2[[i]])
    dftemp<-dftemp %>% 
      dplyr::arrange(dftemp[[1]]) %>% 
      dplyr::mutate(cumhaz = cumsum(dftemp[[2]])) %>% 
      dplyr::mutate(survProp = exp(-1*cumhaz))
    dfSurv2[[i]]<-dftemp[[4]]
  }
  dfSurv2$V16<-surv_MM_lthaz
  dfSurv2$V17<-mixcure_surv_mod1
  prime1<-c(0,rep(1,16))
  dfSurv2<-rbind(prime1,dfSurv2)
  colnames(dfSurv2) <-c("Time",md)
  
  output<-list(dfGOF=dfGOF,ltHaz=ltHaz,dfHazEst=dfHazEst,dfHazEst2=dfHazEst2,dfSurv=dfSurv,dfSurv2=dfSurv2)
  
  return(output)
}
