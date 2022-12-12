#NIV_os
nivos_aiv<-as.data.frame(cbind(seq(1,28,1),myAIC))
nivos_aiv<-arrange(nivos_aiv,myAIC)
dists_MM[nivos_aiv$V1[1],]
dists_MM[nivos_aiv$V1[2],]


mixfit0 <- flexsurvmixture(survgef ~ 1, dists = c("llogis","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
mixfit1 <- flexsurvmixture(survgef ~ 1, dists = c("exp","llogis"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
#1
plot(survgef)
lines(seq.int(max(48)), 1-pllogis(seq.int(48), mixfit0$res[2],mixfit0$res[3]), col = "blue", lwd = 1) 
lines(seq.int(48), 1-pgengamma(seq.int(48), mixfit0$res[4], mixfit0$res[5], mixfit0$res[6]), col = "cyan", lwd = 1)
lines(summary(mixfit0, t = seq.int(48))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv1<-summary(mixfit0, t = Newtime2$Time*12,type="survival")[[1]]$est

#2
plot(survgef)
lines(seq.int(max(48)), 1-pexp(seq.int(48), mixfit1$res[2]), col = "blue", lwd = 1) 
lines(seq.int(48), 1-pllogis(seq.int(48), mixfit1$res[3], mixfit1$res[4]), col = "cyan", lwd = 1)
lines(summary(mixfit1, t = seq.int(48))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv2<-summary(mixfit1, t = Newtime2$Time*12,type="survival")[[1]]$est

test_surv<-data.frame(time=Newtime2$Time,surv1=test_surv1,surv2=test_surv2)

ggplot()+geom_line(data = test_surv,aes(x=time,y=surv1),colour="red",size=1,show.legend = TRUE)+
  geom_line(data = test_surv,aes(x=time,y=surv2),colour="blue",size=1,show.legend = TRUE)+
  labs(y="Surv",x="Time(years)")


# Thus,choose 2, model 1 over-fit


#IPI_os
nivos_aiv<-as.data.frame(cbind(seq(1,28,1),myAIC))
nivos_aiv<-arrange(nivos_aiv,myAIC)
dists_MM[nivos_aiv$V1[1],]
dists_MM[nivos_aiv$V1[2],]


mixfit0 <- flexsurvmixture(survgef ~ 1, dists = c("gengamma","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
mixfit1 <- flexsurvmixture(survgef ~ 1, dists = c("llogis","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
#1
plot(survgef)
lines(seq.int(max(48)), 1-pllogis(seq.int(48), mixfit0$res[2],mixfit0$res[3]), col = "blue", lwd = 1) 
lines(seq.int(48), 1-pgengamma(seq.int(48), mixfit0$res[4], mixfit0$res[5], mixfit0$res[6]), col = "cyan", lwd = 1)
lines(summary(mixfit0, t = seq.int(48))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv1<-summary(mixfit0, t = Newtime2$Time*12,type="survival")[[1]]$est

#  Optimisation has probably not converged to the maximum likelihood - Hessian is not positive definite. 

#2
plot(survgef)
lines(seq.int(max(48)), 1-pexp(seq.int(48), mixfit1$res[2]), col = "blue", lwd = 1) 
lines(seq.int(48), 1-pllogis(seq.int(48), mixfit1$res[3], mixfit1$res[4]), col = "cyan", lwd = 1)
lines(summary(mixfit1, t = seq.int(48))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv2<-summary(mixfit1, t = Newtime2$Time*12,type="survival")[[1]]$est

test_surv<-data.frame(time=Newtime2$Time,surv1=test_surv1,surv2=test_surv2)

ggplot()+geom_line(data = test_surv,aes(x=time,y=surv1),colour="red",size=1,show.legend = TRUE)+
  geom_line(data = test_surv,aes(x=time,y=surv2),colour="blue",size=1,show.legend = TRUE)+
  labs(y="Surv",x="Time(years)")

  


# Thus,choose 2, model 1 not converged

#NIV_pfs
nivos_aiv<-as.data.frame(cbind(seq(1,28,1),myAIC))
nivos_aiv<-arrange(nivos_aiv,myAIC)
dists_MM[nivos_aiv$V1[1],]
dists_MM[nivos_aiv$V1[2],]


mixfit0 <- flexsurvmixture(survgef ~ 1, dists = c("gengamma","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
mixfit1 <- flexsurvmixture(survgef ~ 1, dists = c("lnorm","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
#1
plot(survgef)
lines(seq.int(max(48)), 1-pllogis(seq.int(48), mixfit0$res[2],mixfit0$res[3]), col = "blue", lwd = 1) 
lines(seq.int(48), 1-pgengamma(seq.int(48), mixfit0$res[4], mixfit0$res[5], mixfit0$res[6]), col = "cyan", lwd = 1)
lines(summary(mixfit0, t = seq.int(48))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv1<-summary(mixfit0, t = Newtime2$Time*12,type="survival")[[1]]$est

#2
plot(survgef)
lines(seq.int(max(48)), 1-pexp(seq.int(48), mixfit1$res[2]), col = "blue", lwd = 1) 
lines(seq.int(48), 1-pllogis(seq.int(48), mixfit1$res[3], mixfit1$res[4]), col = "cyan", lwd = 1)
lines(summary(mixfit1, t = seq.int(48))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv2<-summary(mixfit1, t = Newtime2$Time*12,type="survival")[[1]]$est

test_surv<-data.frame(time=Newtime2$Time,surv1=test_surv1,surv2=test_surv2)

ggplot()+geom_line(data = test_surv,aes(x=time,y=surv1),colour="red",size=1,show.legend = TRUE)+
  geom_line(data = test_surv,aes(x=time,y=surv2),colour="blue",size=1,show.legend = TRUE)+
  labs(y="Surv",x="Time(years)")


# Thus,choose 1

#ipi_pfs
nivos_aiv<-as.data.frame(cbind(seq(1,28,1),myAIC))
nivos_aiv<-arrange(nivos_aiv,myAIC)
dists_MM[nivos_aiv$V1[1],]
dists_MM[nivos_aiv$V1[2],]


mixfit0 <- flexsurvmixture(survgef ~ 1, dists = c("llogis","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
mixfit1 <- flexsurvmixture(survgef ~ 1, dists = c("gengamma","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
#1
plot(survgef)
lines(seq.int(max(48)), 1-pllogis(seq.int(48), mixfit0$res[2],mixfit0$res[3]), col = "blue", lwd = 1) 
lines(seq.int(48), 1-pgengamma(seq.int(48), mixfit0$res[4], mixfit0$res[5], mixfit0$res[6]), col = "cyan", lwd = 1)
lines(summary(mixfit0, t = seq.int(48))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv1<-summary(mixfit0, t = Newtime2$Time*12,type="survival")[[1]]$est

#2
plot(survgef)
lines(seq.int(max(48)), 1-pexp(seq.int(48), mixfit1$res[2]), col = "blue", lwd = 1) 
lines(seq.int(48), 1-pllogis(seq.int(48), mixfit1$res[3], mixfit1$res[4]), col = "cyan", lwd = 1)
lines(summary(mixfit1, t = seq.int(48))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv2<-summary(mixfit1, t = Newtime2$Time*12,type="survival")[[1]]$est

test_surv<-data.frame(time=Newtime2$Time,surv1=test_surv1,surv2=test_surv2)

ggplot()+geom_line(data = test_surv,aes(x=time,y=surv1),colour="red",size=1,show.legend = TRUE)+
  geom_line(data = test_surv,aes(x=time,y=surv2),colour="blue",size=1,show.legend = TRUE)+
  labs(y="Surv",x="Time(years)")


# Thus,choose 1



###-------------------------------------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------------------------------------------------------------------------------------------------------###



#NIV_os
nivos_aiv<-as.data.frame(cbind(seq(1,28,1),myAIC))
nivos_aiv<-arrange(nivos_aiv,myAIC)
dists_MM[nivos_aiv$V1[1],]
dists_MM[nivos_aiv$V1[2],]


mixfit0 <- flexsurvmixture(survgef ~ 1, dists = c("lnorm","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
mixfit1 <- flexsurvmixture(survgef ~ 1, dists = c("exp","lnorm"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
#1
plot(survgef)
lines(seq.int(max(80)), 1-plnorm(seq.int(80), mixfit0$res[2],mixfit0$res[3]), col = "blue", lwd = 1) 
lines(seq.int(80), 1-pgengamma(seq.int(80), mixfit0$res[4], mixfit0$res[5], mixfit0$res[6]), col = "cyan", lwd = 1)
lines(summary(mixfit0, t = seq.int(80))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv1<-summary(mixfit0, t = Newtime2$Time*12,type="survival")[[1]]$est

#2
plot(survgef)
lines(seq.int(max(80)), 1-pexp(seq.int(80), mixfit1$res[2]), col = "blue", lwd = 1) 
lines(seq.int(80), 1-plnorm(seq.int(80), mixfit1$res[3], mixfit1$res[4]), col = "cyan", lwd = 1)
lines(summary(mixfit1, t = seq.int(80))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv2<-summary(mixfit1, t = Newtime2$Time*12,type="survival")[[1]]$est

test_surv<-data.frame(time=Newtime2$Time,surv1=test_surv1,surv2=test_surv2)

ggplot()+geom_line(data = test_surv,aes(x=time,y=surv1),colour="red",size=1,show.legend = TRUE)+
  geom_line(data = test_surv,aes(x=time,y=surv2),colour="blue",size=1,show.legend = TRUE)+
  labs(y="Surv",x="Time(years)")


# Thus,choose 2, model 1 over-fit


#IPI_os
nivos_aiv<-as.data.frame(cbind(seq(1,28,1),myAIC))
nivos_aiv<-arrange(nivos_aiv,myAIC)
dists_MM[nivos_aiv$V1[1],]
dists_MM[nivos_aiv$V1[2],]


mixfit0 <- flexsurvmixture(survgef ~ 1, dists = c("lnorm","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
mixfit1 <- flexsurvmixture(survgef ~ 1, dists = c("lnorm","lnorm"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
#1
plot(survgef)
lines(seq.int(max(80)), 1-plnorm(seq.int(80), mixfit0$res[2],mixfit0$res[3]), col = "blue", lwd = 1) 
lines(seq.int(80), 1-pgengamma(seq.int(80), mixfit0$res[4], mixfit0$res[5], mixfit0$res[6]), col = "cyan", lwd = 1)
lines(summary(mixfit0, t = seq.int(80))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv1<-summary(mixfit0, t = Newtime2$Time*12,type="survival")[[1]]$est

#  Optimisation has probably not converged to the maximum likelihood - Hessian is not positive definite. 

#2
plot(survgef)
lines(seq.int(max(80)), 1-plnorm(seq.int(80), mixfit1$res[2], mixfit1$res[3]), col = "blue", lwd = 1) 
lines(seq.int(80), 1-plnorm(seq.int(80), mixfit1$res[4], mixfit1$res[5]), col = "cyan", lwd = 1)
lines(summary(mixfit1, t = seq.int(80))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv2<-summary(mixfit1, t = Newtime2$Time*12,type="survival")[[1]]$est

test_surv<-data.frame(time=Newtime2$Time,surv1=test_surv1,surv2=test_surv2)

ggplot()+geom_line(data = test_surv,aes(x=time,y=surv1),colour="red",size=1,show.legend = TRUE)+
  geom_line(data = test_surv,aes(x=time,y=surv2),colour="blue",size=1,show.legend = TRUE)+
  labs(y="Surv",x="Time(years)")




# Thus,choose 2, model 1 not converged

#NIV_pfs
nivos_aiv<-as.data.frame(cbind(seq(1,28,1),myAIC))
nivos_aiv<-arrange(nivos_aiv,myAIC)
dists_MM[nivos_aiv$V1[1],]
dists_MM[nivos_aiv$V1[2],]

mixfit0 <- flexsurvmixture(survgef ~ 1, dists = c("gengamma","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
mixfit1 <- flexsurvmixture(survgef ~ 1, dists = c("weibull","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))

#1
plot(survgef)
lines(seq.int(max(80)), 1-pgengamma(seq.int(80), mixfit0$res[2],mixfit0$res[3],mixfit0$res[4]), col = "blue", lwd = 1) 
lines(seq.int(80), 1-pgengamma(seq.int(80), mixfit0$res[5], mixfit0$res[6], mixfit0$res[7]), col = "cyan", lwd = 1)
lines(summary(mixfit0, t = seq.int(80))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv1<-summary(mixfit0, t = Newtime2$Time*12,type="survival")[[1]]$est

#2
plot(survgef)
lines(seq.int(max(80)), 1-pweibull(seq.int(80), mixfit1$res[2],mixfit0$res[3]), col = "blue", lwd = 1) 
lines(seq.int(80), 1-pgengamma(seq.int(80), mixfit1$res[4], mixfit0$res[5], mixfit0$res[6]), col = "cyan", lwd = 1)
lines(summary(mixfit1, t = seq.int(80))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv2<-summary(mixfit1, t = Newtime2$Time*12,type="survival")[[1]]$est

test_surv<-data.frame(time=Newtime2$Time,surv1=test_surv1,surv2=test_surv2)

ggplot()+geom_line(data = test_surv,aes(x=time,y=surv1),colour="red",size=1,show.legend = TRUE)+
  geom_line(data = test_surv,aes(x=time,y=surv2),colour="blue",size=1,show.legend = TRUE)+
  labs(y="Surv",x="Time(years)")


# Thus,choose 1

#ipi_pfs
nivos_aiv<-as.data.frame(cbind(seq(1,28,1),myAIC))
nivos_aiv<-arrange(nivos_aiv,myAIC)
dists_MM[nivos_aiv$V1[1],]
dists_MM[nivos_aiv$V1[2],]


mixfit0 <- flexsurvmixture(survgef ~ 1, dists = c("lnorm","lnorm"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
mixfit1 <- flexsurvmixture(survgef ~ 1, dists = c("llogis","gengamma"), control = list(reltol = 1e-8), sr.control= list(reltol = 1e-8))
#1
plot(survgef)
lines(seq.int(max(80)), 1-plnorm(seq.int(80), mixfit0$res[2],mixfit0$res[3]), col = "blue", lwd = 1) 
lines(seq.int(80), 1-plnorm(seq.int(80), mixfit0$res[4], mixfit0$res[5]), col = "cyan", lwd = 1)
lines(summary(mixfit0, t = seq.int(80))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv1<-summary(mixfit0, t = Newtime2$Time*12,type="survival")[[1]]$est

#2
plot(survgef)
lines(seq.int(max(80)), 1-pllogis(seq.int(80), mixfit1$res[2], mixfit1$res[3]), col = "blue", lwd = 1) 
lines(seq.int(80), 1-pgengamma(seq.int(80), mixfit1$res[4], mixfit1$res[5], mixfit0$res[6]), col = "cyan", lwd = 1)
lines(summary(mixfit1, t = seq.int(80))[[1]][,c("time", "est")], col = "red", lwd = 2)

test_surv2<-summary(mixfit1, t = Newtime2$Time*12,type="survival")[[1]]$est

test_surv<-data.frame(time=Newtime2$Time,surv1=test_surv1,surv2=test_surv2)

ggplot()+geom_line(data = test_surv,aes(x=time,y=surv1),colour="red",size=1,show.legend = TRUE)+
  geom_line(data = test_surv,aes(x=time,y=surv2),colour="blue",size=1,show.legend = TRUE)+
  labs(y="Surv",x="Time(years)")


# Thus,choose 1

