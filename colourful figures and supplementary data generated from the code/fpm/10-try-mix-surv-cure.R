# df <- my_df_ipd
# best_wei <- flexsurvcure(Surv(Obs_surv, Censor) ~ 1, data = df, link="logistic", bhazard = Hazard,mixture=T, dist = "weibull")
# myLnL[1] <- best_wei$loglik
# myPram[1]<-best_wei$npars
# myAIC[1] <- best_wei$AIC
# S_wei<-summary(best_wei,type="survival",t=my_time)
# #plot(S_wei[[1]]$time,S_wei[[1]]$est)
# h_wei<-summary(best_wei,type="hazard",t=my_time)
# 
# ### Lnorm CuRe
# df <- my_df_ipd
# best_lnorm <- flexsurvcure(Surv(Obs_surv, Censor) ~ 1, data = df, link="logistic", bhazard = Hazard,mixture=T, dist = "lnorm")
# myLnL[2] <- best_lnorm$loglik
# myPram[2]<-best_lnorm$npars
# myAIC[2] <- best_lnorm$AIC
# S_lnorm<-summary(best_lnorm,type="survival",t=my_time)
# #plot(S_lnorm[[1]]$time,S_lnorm[[1]]$est)
# h_lnorm<-summary(best_lnorm,type="hazard",t=my_time)
# 
# fmc_surv<-data.frame(Time=my_time,weibull=S_wei[[1]]$est,lnorm=S_lnorm[[1]]$est)
# 
# dffmc = fmc_surv %>%
#   gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))


# f_surv_mod1= ggplot() +
#   geom_ribbon(data = data_fit_niv3y,aes(x=time,ymin=lower,ymax=upper),alpha = 0.3,show.legend = FALSE,fill="gray50")+
#   geom_line(data = data_fit_niv3y,aes(x=time,y=lower),linetype = 2,colour="gray50")+
#   geom_line(data=data_fit_niv3y,aes(x=time,y=upper),linetype = 2,colour="gray50")+
#   geom_line(data=dffmc, aes(x=Time, y=survProp, group=Model, colour=Model), size=1.2,show.legend = FALSE) +
#   geom_line(data=reference0, aes(x=Time, y=Surv,colour="KM"), size=0.75,colour="Black")+
#   scale_color_discrete(name="Model")+
#   expand_limits(y=c(0,1),x=c(0,3)) + 
#   facet_wrap(~Model,nrow=4)+
#   scale_x_continuous(breaks = c(seq(from=0, to=3,by = 1))) +
#   ylab("NIV-OS") +
#   xlab("Time(Years)") +
#   guides(color = guide_legend(ncol = 1))  +
#   theme(legend.position = "bottom") 
# f_surv_mod1


df <- my_df_ipd
best_wei <- flexsurvcure(Surv(Obs_surv, Censor) ~ 1, data = df, link="logistic", bhazard = Hazard,mixture=T, dist = "weibull")
myLnL[1] <- best_wei$loglik
myPram[1]<-best_wei$npars
myAIC[1] <- best_wei$AIC
S_wei<-summary(best_wei,type="survival",t=my_time2)
#plot(S_wei[[1]]$time,S_wei[[1]]$est)
h_wei<-summary(best_wei,type="hazard",t=my_time2)

### Lnorm CuRe
df <- my_df_ipd
best_lnorm <- flexsurvcure(Surv(Obs_surv, Censor) ~ 1, data = df, link="logistic", bhazard = Hazard,mixture=T, dist = "lnorm")
myLnL[2] <- best_lnorm$loglik
myPram[2]<-best_lnorm$npars
myAIC[2] <- best_lnorm$AIC
S_lnorm<-summary(best_lnorm,type="survival",t=my_time2)
#plot(S_lnorm[[1]]$time,S_lnorm[[1]]$est)
h_lnorm<-summary(best_lnorm,type="hazard",t=my_time2)

fmc_surv<-data.frame(Time=my_time2,weibull=S_wei[[1]]$est,lnorm=S_lnorm[[1]]$est)

dffmc = fmc_surv %>%
  gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))

f_surv_mod1= ggplot() +
  geom_ribbon(data = data_fit_niv5y,aes(x=time,ymin=lower,ymax=upper),alpha = 0.3,show.legend = FALSE,fill="gray50")+
  geom_line(data = data_fit_niv5y,aes(x=time,y=lower),linetype = 2,colour="gray50")+
  geom_line(data=data_fit_niv5y,aes(x=time,y=upper),linetype = 2,colour="gray50")+
  geom_line(data=dffmc, aes(x=Time, y=survProp, group=Model, colour=Model), size=1.2,show.legend = FALSE) +
  geom_line(data=reference1, aes(x=Time, y=Surv,colour="KM"), size=0.75,colour="Black")+
  scale_color_discrete(name="Model")+
  expand_limits(y=c(0,1),x=c(0,6.5)) + 
  facet_wrap(~Model,nrow=4)+
  scale_x_continuous(breaks = c(seq(from=0, to=6.5,by = 1))) +
  ylab("NIV-OS") +
  xlab("Time(Years)") +
  guides(color = guide_legend(ncol = 1))  +
  theme(legend.position = "bottom") 
f_surv_mod1


# 
# 
# ### weibull CuRe
# df <- my_df_ipd
# best_wei <- fit.cure.model(Surv(Obs_surv, Censor) ~ 1, data = df, bhazard = "Hazard",formula.surv = list(~ 1, ~ 1), dist = "weibull")
# myLnL[1] <- best_wei$ML
# myPram[1]<- length(best_wei$coefs)
# myAIC[1] <- 2*(length(best_wei$coefs) + -best_wei$ML)
# cure_p1 <- predict(best_wei, type = "curerate")[[1]]$Estimate
# #   Save data on hazard and survival for those with disease (uncured)
# df_best_wei <- tibble(Time = my_time, Srv_dis = predict(best_wei, type = "survuncured", time = my_time)[[1]]$Estimate,
#                       Haz_dis = predict(best_wei, type = "hazarduncured", time = my_time)[[1]]$Estimate)
# df_best_wei_mod_1 <- left_join(best_wei, df_GP1, by = "Time") %>% mutate(Srv_mod = Srv_pop * cure_p + Srv_dis * (1 - cure_p),
#                                                            Pred = Haz_pop + ((1-cure_p)*Haz_dis * Srv_dis)/(cure_p+(1-cure_p)*Srv_dis))
# 
# ### Lnorm CuRe
# df <- my_df_ipd
# best_lnorm <- fit.cure.model(Surv(Obs_surv, Censor) ~ 1, data = df, bhazard = "Hazard",formula.surv = list(~ 1, ~ 1), dist = "lognormal")
# myLnL[2] <- best_lnorm$ML
# myPram[2]<- length(best_lnorm$coefs)
# myAIC[2] <- 2*(length(best_lnorm$coefs) + -best_lnorm$ML)
# cure_p2 <- predict(best_lnorm, type = "curerate")[[1]]$Estimate
# #   Save data on hazard and survival for those with disease (uncured)
# df_best_lnorm <- tibble(Time = my_time, Srv_dis = predict(best_lnorm, type = "survuncured", time = my_time)[[1]]$Estimate,
#                         Haz_dis = predict(best_lnorm, type = "hazarduncured", time = my_time)[[1]]$Estimate)
# 
# ### RP CuRe
# max_k<-5
# df <- my_df_ipd
# my_k = 1
# myAICrp <- array(dim=5)
# for(i in 1:max_k){
#   tmp = GenFlexCureModel(Surv(Obs_surv, Censor) ~ 1, data = df, bhazard = "Hazard", df= i, verbose = FALSE)
#   tmp_AIC = 2*(length(tmp$coefs) + length(tmp$coefs.spline) + tmp$NegMaxLik)
#   myAICrp[i]<-tmp_AIC
# }
# kmin<-which.min(myAICrp)
# 
# best_RP<-GenFlexCureModel(Surv(Obs_surv, Censor) ~ 1, data = df, bhazard = "Hazard", df= kmin, verbose = FALSE)
# myAIC[3]<-2*(length(best_RP$coefs) + length(best_RP$coefs.spline) + best_RP$NegMaxLik)
# myLnL[3]<--best_RP$NegMaxLik
# myPram[3]<-length(best_RP$coefs) + length(best_RP$coefs.spline)
# cure_p_mc <- predict(best_RP, type = "curerate")[[1]]$Estimate
# # Save data on hazard and survival for those with disease (uncured)
# df_RPcure = tibble(Time = my_time, Srv_dis = predict(best_RP, type = "survuncured", time = my_time)[[1]]$Estimate,
#                    Haz_dis = predict(best_RP, type = "hazarduncured", time = my_time)[[1]]$Estimate)





cure_surv1<-data.frame(time=ltHaz$Time,surv=mixcure_surv_mod1)

f_surv_mod1= ggplot() +
  geom_ribbon(data = data_fit_niv3y,aes(x=time,ymin=lower,ymax=upper),alpha = 0.3,show.legend = FALSE,fill="gray50")+
  geom_line(data = data_fit_niv3y,aes(x=time,y=lower),linetype = 2,colour="gray50")+
  geom_line(data=data_fit_niv3y,aes(x=time,y=upper),linetype = 2,colour="gray50")+
  geom_line(data=cure_surv1, aes(x=time, y=surv), size=1.2,show.legend = FALSE,colour="red") +
  geom_line(data=reference0, aes(x=Time, y=Surv,colour="KM"), size=0.75,colour="Black")+
  # scale_color_discrete(name="Model")+
  expand_limits(y=c(0,1),x=c(0,3)) +
  # facet_wrap(~Model,nrow=4)+
  scale_x_continuous(breaks = c(seq(from=0, to=3,by = 1))) +
  ylab("NIV-OS") +
  xlab("Time(Years)") +
  guides(color = guide_legend(ncol = 1))  +
  theme(legend.position = "bottom")
f_surv_mod1






