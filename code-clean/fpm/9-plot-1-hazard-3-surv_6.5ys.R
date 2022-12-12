##plot lthaz_haz
###
dfHaz2<-ltHaz_out[,11:26]
colnames(dfHaz2) <- c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","FP1","FP2","RCS","RP-hazard","RP-odds","RP-normal","GAM","param-mix","mix-cure")
#,"mix-cure","non-mix-cure"
dfHaz2 <- cbind(data.frame(ltHaz_out$Time), dfHaz2)
dfFig <- dfHaz2
dfFig = dfFig %>% mutate(Time = ltHaz_out.Time) %>% select(-ltHaz_out.Time) %>%
  gather(key = "Model", value = "Haz", -Time) %>% mutate(Model = factor(Model))
dfFig$Model = fct_recode(dfFig$Model, "lognormal"="lnorm", "loglogistic"="llogis")

fighr <- ggplot() +
  geom_point(data=ltHaz_out, aes(x=Time, y=hazLT, size=AtRisk), shape = 1) +  
  geom_line(data=dfFig, aes(x=Time, y=Haz, group=Model, colour=Model), size=0.8,show.legend = FALSE) + 
  labs(x="Time(Years)", y="Hazard")  + guides(size="none") +
  theme(legend.position = "bottom") + coord_cartesian(ylim=c(0,0.15), xlim=c(0,3))+
  expand_limits(y=c(0,1),x=c(0,6.5)) + 
  facet_wrap(~Model,nrow=4)
fighr

#plot ltHaz_out_surv_dfsurv2
###

dfFigSurv2 = dfSurv5 %>%
  gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))

reference0<-data.frame(Time=data_fit_niv5y$time,Surv=data_fit_niv5y$surv)
prime2<-c(0,1)
reference0<-rbind(prime2,reference0)

f_surv_mod1= ggplot() +
  geom_ribbon(data = data_fit_niv5y,aes(x=time,ymin=lower,ymax=upper),alpha = 0.3,show.legend = FALSE,fill="gray50")+
  geom_line(data = data_fit_niv5y,aes(x=time,y=lower),linetype = 2,colour="gray50")+
  geom_line(data=data_fit_niv5y,aes(x=time,y=upper),linetype = 2,colour="gray50")+
  geom_line(data=dfFigSurv2, aes(x=Time, y=survProp, group=Model, colour=Model), size=1.2,show.legend = FALSE) +
  geom_line(data=reference0, aes(x=Time, y=Surv,colour="KM"), size=0.75,colour="Black")+
  scale_color_discrete(name="Model")+
  expand_limits(y=c(0,1),x=c(0,6.5)) + 
  facet_wrap(~Model,nrow=4)+
  scale_x_continuous(breaks = c(seq(from=0, to=6.5,by = 1))) +
  ylab("NIV-OS") +
  xlab("Time(Years)") +
  guides(color = guide_legend(ncol = 1))  +
  theme(legend.position = "bottom") 
f_surv_mod1


#plot dfhazest2_surv_dfsurv_20


dfFigSurv3 = dfSurv4 %>%
  gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))

# reference1<-data.frame(Time=ltHaz_out$Time,Surv=ltHaz_out$surv)
# prime2<-c(0,1)
# reference1<-rbind(prime2,reference1)

f_surv_mod3= ggplot() +
  geom_ribbon(data = data_fit_niv5y,aes(x=time,ymin=lower,ymax=upper),alpha = 0.3,show.legend = FALSE,fill="gray50")+
  geom_line(data = data_fit_niv5y,aes(x=time,y=lower),linetype = 2,colour="gray50")+
  geom_line(data=data_fit_niv5y,aes(x=time,y=upper),linetype = 2,colour="gray50")+
  geom_line(data=dfFigSurv3, aes(x=Time, y=survProp, group=Model, colour=Model), size=1.2,show.legend = FALSE) +
  geom_line(data=reference0, aes(x=Time, y=Surv,colour="KM"), size=0.75,colour="Black")+
  scale_color_discrete(name="Model")+
  expand_limits(y=c(0,1),x=c(0,20)) + 
  facet_wrap(~Model,nrow=4)+
  scale_x_continuous(breaks = c(seq(from=0, to=20,by = 2))) +
  ylab("NIV-OS") +
  xlab("Time(Years)") +
  guides(color = guide_legend(ncol = 1))  +
  theme(legend.position = "bottom") 
f_surv_mod3

#MSE
MSE<-as.data.frame(matrix(nrow = 1,ncol = 16))
colnames(MSE) <- md
for (i in 1:16) {
  MSE[i]<-mean((dfSurv5[,i+1]-ltHaz_out$surv)^2)
}
MSE<-MSE*10000

dfplotmse1<-data.frame(name=colnames(MSE),MSE=t(MSE[1,]))
colnames(dfplotmse1)<-c("name","MSE")
MSE_legend<-as.character(round(MSE[1,],digits=2)) 
fig_mse_1<-ggplot(data=dfplotmse1,mapping=aes(x=name,y=MSE,fill=name,group=factor(1)))+
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label = MSE_legend, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
fig_mse_1


row.names(MSE)<-c("6.5years")


#bias
bias<-as.data.frame(matrix(nrow = 1,ncol = 16))
colnames(bias) <- md
for (i in 1:16) {
  bias[i]<-mean((dfSurv5[,i+1]-ltHaz_out$surv))
}
bias<-bias

dfplotbias1<-data.frame(name=colnames(bias),bias=t(bias[1,]))
colnames(dfplotbias1)<-c("name","bias")
bias_legend<-as.character(round(bias[1,],digits=2)) 
fig_bias_1<-ggplot(data=dfplotbias1,mapping=aes(x=name,y=bias,fill=name,group=factor(1)))+
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label = bias_legend, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
fig_bias_1


row.names(bias)<-c("6.5years")

#auc
auc<-as.data.frame(matrix(nrow = 1,ncol = 16))
colnames(auc) <- md
for (i in 1:16) {
  auc[i]<-sum(abs(dfSurv5[,i+1]-ltHaz_out$surv))
}
auc<-auc

dfplotauc1<-data.frame(name=colnames(auc),auc=t(auc[1,]))
colnames(dfplotauc1)<-c("name","auc")
auc_legend<-as.character(round(auc[1,],digits=2)) 
fig_auc_1<-ggplot(data=dfplotauc1,mapping=aes(x=name,y=auc,fill=name,group=factor(1)))+
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label = auc_legend, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
fig_auc_1


row.names(auc)<-c("6.5years")

