##plot lthaz_haz
###
dfHaz2<-ltHaz[,11:26]
colnames(dfHaz2) <- c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","FP1","FP2","RCS","RP-hazard","RP-odds","RP-normal","GAM","param-mix","mix-cure")
#,"mix-cure","non-mix-cure"
dfHaz2 <- cbind(data.frame(ltHaz$Time), dfHaz2)
dfFig <- dfHaz2
dfFig = dfFig %>% mutate(Time = ltHaz.Time) %>% select(-ltHaz.Time) %>%
  gather(key = "Model", value = "Haz", -Time) %>% mutate(Model = factor(Model))
dfFig$Model = fct_recode(dfFig$Model, "lognormal"="lnorm", "loglogistic"="llogis")

fighr <- ggplot() +
  geom_point(data=ltHaz, aes(x=Time, y=hazLT, size=AtRisk), shape = 1) +  
  geom_line(data=dfFig, aes(x=Time, y=Haz, group=Model, colour=Model), size=0.8,show.legend = FALSE) + 
  labs(x="Time(Years)", y="Hazard")  + guides(size="none") +
  theme(legend.position = "bottom") + coord_cartesian(ylim=c(0,0.15), xlim=c(0,3))+
  expand_limits(y=c(0,1),x=c(0,3)) + 
  facet_wrap(~Model,nrow=4)
fighr

#plot lthaz_surv_dfsurv2
###

dfFigSurv2 = dfSurv2 %>%
  gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))

reference0<-data.frame(Time=data_fit_niv3y$time,Surv=data_fit_niv3y$surv)
prime2<-c(0,1)
reference0<-rbind(prime2,reference0)

f_surv_mod1= ggplot() +
  geom_ribbon(data = data_fit_niv3y,aes(x=time,ymin=lower,ymax=upper),alpha = 0.3,show.legend = FALSE,fill="gray50")+
  geom_line(data = data_fit_niv3y,aes(x=time,y=lower),linetype = 2,colour="gray50")+
  geom_line(data=data_fit_niv3y,aes(x=time,y=upper),linetype = 2,colour="gray50")+
  geom_line(data=dfFigSurv2, aes(x=Time, y=survProp, group=Model, colour=Model), size=1.2,show.legend = FALSE) +
  geom_line(data=reference0, aes(x=Time, y=Surv,colour="KM"), size=0.75,colour="Black")+
  scale_color_discrete(name="Model")+
  expand_limits(y=c(0,1),x=c(0,3)) + 
  facet_wrap(~Model,nrow=4)+
  scale_x_continuous(breaks = c(seq(from=0, to=3,by = 1))) +
  ylab("NIV-OS") +
  xlab("Time(Years)") +
  guides(color = guide_legend(ncol = 1))  +
  theme(legend.position = "bottom") 
f_surv_mod1

#plot dfhazest2_surv_dfsurv_6.5

dfSurv3<-dfSurv[1:79,]
dfFigSurv = dfSurv3 %>%
  gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))

reference1<-data.frame(Time=data_fit_niv5y$time,Surv=data_fit_niv5y$surv)
prime2<-c(0,1)
reference1<-rbind(prime2,reference1)

f_surv_mod2= ggplot() +
  geom_ribbon(data = data_fit_niv5y,aes(x=time,ymin=lower,ymax=upper),alpha = 0.3,show.legend = FALSE,fill="gray50")+
  geom_line(data = data_fit_niv5y,aes(x=time,y=lower),linetype = 2,colour="gray50")+
  geom_line(data=data_fit_niv5y,aes(x=time,y=upper),linetype = 2,colour="gray50")+
  geom_line(data=dfFigSurv, aes(x=Time, y=survProp, group=Model, colour=Model), size=1.2,show.legend = FALSE) +
  geom_line(data=reference1, aes(x=Time, y=Surv,colour="KM"), size=0.75,colour="Black")+
  scale_color_discrete(name="Model")+
  expand_limits(y=c(0,1),x=c(0,6.5)) + 
  facet_wrap(~Model,nrow=4)+
  scale_x_continuous(breaks = c(seq(from=0, to=6.5,by = 1))) +
  ylab("NIV-OS") +
  xlab("Time(Years)") +
  guides(color = guide_legend(ncol = 1))  +
  theme(legend.position = "bottom") 
f_surv_mod2

#plot dfhazest2_surv_dfsurv_20

dfFigSurv3 = dfSurv %>%
  gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))


f_surv_mod3= ggplot() +
  geom_ribbon(data = data_fit_niv5y,aes(x=time,ymin=lower,ymax=upper),alpha = 0.3,show.legend = FALSE,fill="gray50")+
  geom_line(data = data_fit_niv5y,aes(x=time,y=lower),linetype = 2,colour="gray50")+
  geom_line(data=data_fit_niv5y,aes(x=time,y=upper),linetype = 2,colour="gray50")+
  geom_line(data=dfFigSurv3, aes(x=Time, y=survProp, group=Model, colour=Model), size=1.2,show.legend = FALSE) +
  geom_line(data=reference1, aes(x=Time, y=Surv,colour="KM"), size=0.75,colour="Black")+
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
  MSE[i]<-mean((dfSurv2[,i+1]-ltHaz$surv)^2)
}
MSE<-MSE*10000

dfplotmse1<-data.frame(name=colnames(MSE),MSE=t(MSE[1,]))
colnames(dfplotmse1)<-c("name","MSE")
MSE_legend<-as.character(round(MSE[1,],digits=2)) 
fig_mse_1<-ggplot(data=dfplotmse1,mapping=aes(x=name,y=MSE,fill=name,group=factor(1)))+
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label = MSE_legend, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
fig_mse_1


# dfhazest 6.5 years data 
MSE2<-as.data.frame(matrix(nrow = 1,ncol = 16))
colnames(MSE2) <- md
for (i in 1:16) {
  MSE2[i]<-mean((dfSurv3[,i+1]-ltHaz_out$surv)^2)
}
MSE2<-MSE2*10000

dfplotmse2<-data.frame(name=colnames(MSE2),MSE2=t(MSE2[1,]))
colnames(dfplotmse2)<-c("name","MSE")
MSE_legend2<-as.character(round(MSE2[1,],digits=2)) 
fig_mse_2<-ggplot(data=dfplotmse2,mapping=aes(x=name,y=MSE,fill=name,group=factor(1)))+
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label = MSE_legend2, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
fig_mse_2

MSE_final<-rbind(MSE,MSE2)
row.names(MSE_final)<-c("3years","6.5years")


#Bias
bias<-as.data.frame(matrix(nrow = 1,ncol = 16))
colnames(bias) <- md
for (i in 1:16) {
  bias[i]<-mean((dfSurv2[,i+1]-ltHaz$surv))
}
bias<-bias

dfplotbias1<-data.frame(name=colnames(bias),bias=t(bias[1,]))
colnames(dfplotbias1)<-c("name","bias")
bias_legend<-as.character(round(bias[1,],digits=2)) 
fig_bias_1<-ggplot(data=dfplotbias1,mapping=aes(x=name,y=bias,fill=name,group=factor(1)))+
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label = bias_legend, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
fig_bias_1


# dfhazest 6.5 years data 
bias2<-as.data.frame(matrix(nrow = 1,ncol = 16))
colnames(bias2) <- md
for (i in 1:16) {
  bias2[i]<-mean((dfSurv3[,i+1]-ltHaz_out$surv))
}
bias2<-bias2

dfplotbias2<-data.frame(name=colnames(bias2),bias2=t(bias2[1,]))
colnames(dfplotbias2)<-c("name","bias")
bias_legend2<-as.character(round(bias2[1,],digits=2)) 
fig_bias_2<-ggplot(data=dfplotbias2,mapping=aes(x=name,y=bias,fill=name,group=factor(1)))+
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label = bias_legend2, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
fig_bias_2

bias_final<-rbind(bias,bias2)
row.names(bias_final)<-c("3years","6.5years")



#auc
auc<-as.data.frame(matrix(nrow = 1,ncol = 16))
colnames(auc) <- md
for (i in 1:16) {
  auc[i]<-sum(abs(dfSurv2[,i+1]-ltHaz$surv))
}
auc<-auc

dfplotauc1<-data.frame(name=colnames(auc),auc=t(auc[1,]))
colnames(dfplotauc1)<-c("name","auc")
auc_legend<-as.character(round(auc[1,],digits=2)) 
fig_auc_1<-ggplot(data=dfplotauc1,mapping=aes(x=name,y=auc,fill=name,group=factor(1)))+
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label = auc_legend, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
fig_auc_1


# dfhazest 6.5 years data 
auc2<-as.data.frame(matrix(nrow = 1,ncol = 16))
colnames(auc2) <- md
for (i in 1:16) {
  auc2[i]<-sum(abs(dfSurv3[,i+1]-ltHaz_out$surv))
}
auc2<-auc2

dfplotauc2<-data.frame(name=colnames(auc2),auc2=t(auc2[1,]))
colnames(dfplotauc2)<-c("name","auc")
auc_legend2<-as.character(round(auc2[1,],digits=2)) 
fig_auc_2<-ggplot(data=dfplotauc2,mapping=aes(x=name,y=auc,fill=name,group=factor(1)))+
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label = auc_legend2, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
fig_auc_2

auc_final<-rbind(auc,auc2)
row.names(auc_final)<-c("3years","6.5years")

