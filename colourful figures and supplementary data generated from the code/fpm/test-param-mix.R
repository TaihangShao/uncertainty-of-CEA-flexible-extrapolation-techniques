sd_bc<-data.frame("recyrs"=gef_in$recyrs,"censrec"=gef_in$censrec)
Newtime$Time<-exp(Newtime$lnTime)

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
plot(survgef)

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
which.min(myAIC)
distMM_best<-dists_MM[which.min(myAIC),]##gompertz,gengamma,best;AIC=254.95
# MMres<-cbind(dists_MM,myAIC)
# MMres<-arrange(MMres,myAIC) 

# best mixture model
mixfit <- flexsurvmixture(survgef ~ 1, dists = c(distMM_best$dist1,distMM_best$dist2), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
#mixfit <- flexsurvmixture(survgef ~ 1, dists = c("llogis","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
# Plot mixture model
## KM curve
plot(survgef)
## Component 1: weibull
lines(seq.int(max(48)), 1-plnorm(seq.int(48), mixfit$res[2],mixfit$res[3]), col = "blue", lwd = 1)
## Component 2: gengamma
lines(seq.int(48), 1-pgompertz(seq.int(48), mixfit$res[4], mixfit$res[5]), col = "cyan", lwd = 1)
## Mixture model
lines(summary(mixfit, t = seq.int(48))[[1]][,c("time", "est")], col = "red", lwd = 2)
# write data
MODi<-15

dfHazEst[MODi,]<-summary(mixfit, t = Newtime$Time*12,type="hazard")[[1]]$est
dfHazEst2[MODi,]<-summary(mixfit, t = Newtime2$Time*12,type="hazard")[[1]]$est
ltHaz[md[MODi]] <- summary(mixfit, t=ltHaz$Time*12,type="hazard")[[1]]$est

dfGOF[MODi,1] <- modnames[[8]]
dfGOF[MODi,2] <- sum(ltHaz$Events*log(ltHaz[modnames[[8]]]) - ltHaz[[modnames[[8]]]]*ltHaz$AtRisk) + llCons
dfGOF[MODi,3] <- mixfit$npars 
coeff_temp<-as.data.frame(mixfit$coefficients)
t1<-"Param"
for (i in 1:length(mixfit$coefficients)) {
  t0<-paste(rownames(coeff_temp)[i],round(coeff_temp[i,1],2),sep = "=")
  t1<-paste(t1,t0,sep = ";")
}
dfGOF[MODi,5] <- paste(paste(distMM_best$dist1,distMM_best$dist2,sep = "-"),t1,sep = ":") 




