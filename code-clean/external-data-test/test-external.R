IPD1 <- read.delim("IPD1.txt")

ipi10y <- IPD1
ipi10y<-data.frame(ipi10y$event,ipi10y$time)
ipi10y<-rename(ipi10y,"censrec"="ipi10y.event","recyrs"="ipi10y.time")
ipi10y$recyrs<-ipi10y$recyrs/12
ipi10y$rectime<-as.integer(ipi10y$recyrs*365.24) 

ipi10y = ipi10y %>% mutate(censrec = case_when(ipi10y$recyrs > 10 ~ integer(1), TRUE ~ ipi10y$censrec),
                         recyrs = case_when(ipi10y$recyrs > 10 ~ 10, TRUE ~ ipi10y$recyrs),
                         rectime = recyrs * 365)


ipi10y$rectime2 <- as.integer(ipi10y$rectime/(365.24/12)) + 1
#+1 above as integer rounds down, we want to round up
ltBC_ipi10 <- lifeTable(ipi10y, timeColumn = "rectime2", eventColumn = "censrec")
ltHaz0_ipi10 <- data.frame(hazKM = ltBC_ipi10$Output$hazard, Time = (seq(1:length(ltBC_ipi10$Output[,1]))-0.5)/12,
                     AtRisk = ltBC_ipi10$Output$atRisk, Events = ltBC_ipi10$Output$events)
# The above hazard is the product-limit (KM) estimate. Also calculate the life-table (acturial) estimate
ltHaz0_ipi10$hazLT = ltHaz0_ipi10$Events / (ltHaz0_ipi10$AtRisk - ltHaz0_ipi10$Events/2)
# Generate log-time
ltHaz0_ipi10$lnTime <- log(ltHaz0_ipi10$Time)
# For random effects add an ID for each time period
ltHaz0_ipi10$MyId <- 1:dim(ltHaz0_ipi10)[1] # Generate id variable 
# ltHaz0_ipi10$re<-scale(ltHaz0_ipi10$MyId)
# For AR(1) model get outcomes lagged by one.
ltHaz0_ipi10$EventsL <- lag(ltHaz0_ipi10$Events)
# Set first lagged value = 0 (usually would discard, but retain so IC are comparable. Can be justified as a prior value)
ltHaz0_ipi10$EventsL[1] <- 0
#Set surv data
ltHaz0_ipi10$surv <- ltBC_ipi10$Output$S
#timedelta
ltHaz0_ipi10$timedelta<-ltHaz0_ipi10$Time[2]-ltHaz0_ipi10$Time[1]


#3ys data
dfFigSurv3 = dfSurv[1:121,] %>%
  gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))

fit_ipi10<-survfit(Surv(IPD1$time/12,IPD1$event)~1,data=IPD1,conf.int=0.9999)
data_fit_ipi10<-data.frame(time=fit_ipi10$time, surv=fit_ipi10$surv,lower=fit_ipi10$lower,upper=fit_ipi10$upper)

reference0<-data.frame(Time=data_fit_niv3y$time,Surv=data_fit_niv3y$surv)
prime2<-c(0,1)
reference0<-rbind(prime2,reference0)

reference1<-data.frame(Time=fit_ipi10$time ,Surv=fit_ipi10$surv)
prime2<-c(0,1)
reference1<-rbind(prime2,reference1)

# f_surv_mod3= ggplot() +
#   geom_ribbon(data = data_fit_niv3y,aes(x=time,ymin=lower,ymax=upper),alpha = 0.3,show.legend = FALSE,fill="gray50")+
#   geom_line(data = data_fit_niv3y,aes(x=time,y=lower),linetype = 2,colour="gray50")+
#   geom_line(data=data_fit_niv3y,aes(x=time,y=upper),linetype = 2,colour="gray50")+
#   geom_line(data=reference0, aes(x=Time, y=Surv,colour="KM"), size=0.75,colour="Black")+
#   geom_ribbon(data = data_fit_ipi10,aes(x=time,ymin=lower,ymax=upper),alpha = 0.3,show.legend = FALSE,fill="gray50")+
#   geom_line(data = data_fit_ipi10,aes(x=time,y=lower),linetype = 2,colour="gray50")+
#   geom_line(data=data_fit_ipi10,aes(x=time,y=upper),linetype = 2,colour="gray50")+
#   geom_line(data=dfFigSurv3, aes(x=Time, y=survProp, group=Model, colour=Model), size=1.2,show.legend = FALSE) +
#   geom_vline(xintercept=3,size=1,linetype=2)+
#   geom_line(data=reference1, aes(x=Time, y=Surv,colour="KM"), size=0.75,colour="Black")+
#   scale_color_discrete(name="Model")+
#   expand_limits(y=c(0,1),x=c(0,10)) + 
#   facet_wrap(~Model,nrow=4)+
#   scale_x_continuous(breaks = c(seq(from=0, to=10,by = 2))) +
#   ylab("NIV-OS") +
#   xlab("Time(Years)") +
#   guides(color = guide_legend(ncol = 1))  +
#   theme(legend.position = "bottom") 
# f_surv_mod3
# 
# ggsave(f_surv_mod3,filename = "3ys-external-data-test.png",width = 18,height = 12,dpi = 800)

# #MSE
# cal_df<-dfSurv[38:120,]
# cal_lthaz<-ltHaz0_ipi10[37:119,]
# cal_lthaz<-data.frame(Time=cal_lthaz$Time,Surv=cal_lthaz$surv)
# 
# MSE<-as.data.frame(matrix(nrow = 1,ncol = 16))
# colnames(MSE) <- md
# for (i in 1:16) {
#   MSE[i]<-mean((cal_df[,i+1]-cal_lthaz$Surv)^2)
# }
# MSE<-MSE*10000
# 
# dfplotmse1<-data.frame(name=colnames(MSE),MSE=t(MSE[1,]))
# colnames(dfplotmse1)<-c("name","MSE")
# MSE_legend<-as.character(round(MSE[1,],digits=2))
# fig_mse_1<-ggplot(data=dfplotmse1,mapping=aes(x=name,y=MSE,fill=name,group=factor(1)))+
#   geom_bar(stat="identity",width=0.8)+
#   geom_text(aes(label = MSE_legend, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
# fig_mse_1
# 
# 
# # row.names(MSE)<-c("6.5years")
# write.csv(MSE,file = "3ys-external-data-test-mse.csv")
# ggsave(fig_mse_1,filename = "3ys-external-data-test-mse-1.png",width = 18,height = 12,dpi = 800)



#bias
cal_df<-dfSurv[38:120,]
cal_lthaz<-ltHaz0_ipi10[37:119,]
cal_lthaz<-data.frame(Time=cal_lthaz$Time,Surv=cal_lthaz$surv)

bias<-as.data.frame(matrix(nrow = 1,ncol = 16))
colnames(bias) <- md
for (i in 1:16) {
  bias[i]<-mean((cal_df[,i+1]-cal_lthaz$Surv))
}
bias<-bias

dfplotbias1<-data.frame(name=colnames(bias),bias=t(bias[1,]))
colnames(dfplotbias1)<-c("name","bias")
bias_legend<-as.character(round(bias[1,],digits=2))
fig_bias_1<-ggplot(data=dfplotbias1,mapping=aes(x=name,y=bias,fill=name,group=factor(1)))+
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label = bias_legend, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
fig_bias_1


# row.names(bias)<-c("6.5years")
write.csv(bias,file = "3ys-external-data-test-bias.csv")
ggsave(fig_bias_1,filename = "3ys-external-data-test-bias-1.png",width = 18,height = 12,dpi = 800)

#auc
cal_df<-dfSurv[38:120,]
cal_lthaz<-ltHaz0_ipi10[37:119,]
cal_lthaz<-data.frame(Time=cal_lthaz$Time,Surv=cal_lthaz$surv)

auc<-as.data.frame(matrix(nrow = 1,ncol = 16))
colnames(auc) <- md
for (i in 1:16) {
  auc[i]<-sum(abs(cal_df[,i+1]-cal_lthaz$Surv))
}
auc<-auc

dfplotauc1<-data.frame(name=colnames(auc),auc=t(auc[1,]))
colnames(dfplotauc1)<-c("name","auc")
auc_legend<-as.character(round(auc[1,],digits=2))
fig_auc_1<-ggplot(data=dfplotauc1,mapping=aes(x=name,y=auc,fill=name,group=factor(1)))+
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label = auc_legend, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
fig_auc_1


# row.names(auc)<-c("6.5years")
write.csv(auc,file = "3ys-external-data-test-auc.csv")
ggsave(fig_auc_1,filename = "3ys-external-data-test-auc-1.png",width = 18,height = 12,dpi = 800)




#6.5ys data
dfFigSurv3 = dfSurv4[1:121,] %>%
  gather(key = "Model", value = "survProp", -Time) %>% mutate(Model = factor(Model))

fit_ipi10<-survfit(Surv(IPD1$time/12,IPD1$event)~1,data=IPD1,conf.int=0.9999)
data_fit_ipi10<-data.frame(time=fit_ipi10$time, surv=fit_ipi10$surv,lower=fit_ipi10$lower,upper=fit_ipi10$upper)

reference0<-data.frame(Time=data_fit_niv5y$time,Surv=data_fit_niv5y$surv)
prime2<-c(0,1)
reference0<-rbind(prime2,reference0)

reference1<-data.frame(Time=fit_ipi10$time ,Surv=fit_ipi10$surv)
prime2<-c(0,1)
reference1<-rbind(prime2,reference1)

# 
# 
# f_surv_mod3= ggplot() +
#   geom_ribbon(data = data_fit_niv3y,aes(x=time,ymin=lower,ymax=upper),alpha = 0.3,show.legend = FALSE,fill="gray50")+
#   geom_line(data = data_fit_niv3y,aes(x=time,y=lower),linetype = 2,colour="gray50")+
#   geom_line(data=data_fit_niv3y,aes(x=time,y=upper),linetype = 2,colour="gray50")+
#   geom_line(data=reference0, aes(x=Time, y=Surv,colour="KM"), size=0.75,colour="Black")+
#   geom_ribbon(data = data_fit_ipi10,aes(x=time,ymin=lower,ymax=upper),alpha = 0.3,show.legend = FALSE,fill="gray50")+
#   geom_line(data = data_fit_ipi10,aes(x=time,y=lower),linetype = 2,colour="gray50")+
#   geom_line(data=data_fit_ipi10,aes(x=time,y=upper),linetype = 2,colour="gray50")+
#   geom_line(data=dfFigSurv3, aes(x=Time, y=survProp, group=Model, colour=Model), size=1.2,show.legend = FALSE) +
#   geom_line(data=reference1, aes(x=Time, y=Surv,colour="KM"), size=0.75,colour="Black")+
#   geom_vline(xintercept=6.5,size=1,linetype=2)+
#   # geom_line(data=ltHaz0_ipi10,aes(x=Time,y=surv),colour="red",linetype=4)+
#   scale_color_discrete(name="Model")+
#   expand_limits(y=c(0,1),x=c(0,10)) + 
#   facet_wrap(~Model,nrow=4)+
#   scale_x_continuous(breaks = c(seq(from=0, to=10,by = 2))) +
#   ylab("NIV-OS") +
#   xlab("Time(Years)") +
#   guides(color = guide_legend(ncol = 1))  +
#   theme(legend.position = "bottom") 
# f_surv_mod3
# 
# ggsave(f_surv_mod3,filename = "6.5ys-external-data-test.png",width = 18,height = 12,dpi = 800)

# #MSE
# cal_df<-dfSurv4[80:120,]
# cal_lthaz<-ltHaz0_ipi10[79:119,]
# cal_lthaz<-data.frame(Time=cal_lthaz$Time,Surv=cal_lthaz$surv)
# 
# MSE<-as.data.frame(matrix(nrow = 1,ncol = 16))
# colnames(MSE) <- md
# for (i in 1:16) {
#   MSE[i]<-mean((cal_df[,i+1]-cal_lthaz$Surv)^2)
# }
# MSE<-MSE*10000
# 
# dfplotmse1<-data.frame(name=colnames(MSE),MSE=t(MSE[1,]))
# colnames(dfplotmse1)<-c("name","MSE")
# MSE_legend<-as.character(round(MSE[1,],digits=2))
# fig_mse_1<-ggplot(data=dfplotmse1,mapping=aes(x=name,y=MSE,fill=name,group=factor(1)))+
#   geom_bar(stat="identity",width=0.8)+
#   geom_text(aes(label = MSE_legend, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
# fig_mse_1
# 
# 
# # row.names(MSE)<-c("6.5years")
# write.csv(MSE,file = "6.5ys-external-data-test-mse.csv")
# ggsave(fig_mse_1,filename = "6.5ys-external-data-test-mse-1.png",width = 18,height = 12,dpi = 800)

#bias
cal_df<-dfSurv4[80:120,]
cal_lthaz<-ltHaz0_ipi10[79:119,]
cal_lthaz<-data.frame(Time=cal_lthaz$Time,Surv=cal_lthaz$surv)

bias<-as.data.frame(matrix(nrow = 1,ncol = 16))
colnames(bias) <- md
for (i in 1:16) {
  bias[i]<-mean((cal_df[,i+1]-cal_lthaz$Surv))
}
bias<-bias

dfplotbias1<-data.frame(name=colnames(bias),bias=t(bias[1,]))
colnames(dfplotbias1)<-c("name","bias")
bias_legend<-as.character(round(bias[1,],digits=2))
fig_bias_1<-ggplot(data=dfplotbias1,mapping=aes(x=name,y=bias,fill=name,group=factor(1)))+
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label = bias_legend, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
fig_bias_1


# row.names(bias)<-c("6.5years")
write.csv(bias,file = "6.5ys-external-data-test-bias.csv")
ggsave(fig_bias_1,filename = "6.5ys-external-data-test-bias-1.png",width = 18,height = 12,dpi = 800)


#auc
cal_df<-dfSurv4[80:120,]
cal_lthaz<-ltHaz0_ipi10[79:119,]
cal_lthaz<-data.frame(Time=cal_lthaz$Time,Surv=cal_lthaz$surv)

auc<-as.data.frame(matrix(nrow = 1,ncol = 16))
colnames(auc) <- md
for (i in 1:16) {
  auc[i]<-sum(abs(cal_df[,i+1]-cal_lthaz$Surv))
}
auc<-auc

dfplotauc1<-data.frame(name=colnames(auc),auc=t(auc[1,]))
colnames(dfplotauc1)<-c("name","auc")
auc_legend<-as.character(round(auc[1,],digits=2))
fig_auc_1<-ggplot(data=dfplotauc1,mapping=aes(x=name,y=auc,fill=name,group=factor(1)))+
  geom_bar(stat="identity",width=0.8)+
  geom_text(aes(label = auc_legend, vjust = -0.8, hjust = 0.5, color = name), show.legend = TRUE)
fig_auc_1


# row.names(auc)<-c("6.5years")
write.csv(auc,file = "6.5ys-external-data-test-auc.csv")
ggsave(fig_auc_1,filename = "6.5ys-external-data-test-auc-1.png",width = 18,height = 12,dpi = 800)


