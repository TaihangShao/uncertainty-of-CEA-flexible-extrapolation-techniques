

ICER_res_new<-read.csv("ICER_6.5fit_6.5.csv")
ICER_res_new<-read.csv("ICER_6.5fit_20.csv")
ICER_res_new<-read.csv("ICER_3fit_6.5.csv")
ICER_res_new<-read.csv("ICER_3fit_20.csv")
ICER_res_new<-read.csv("ICER_3ex_6.5.csv")
ICER_res_new<-read.csv("ICER_3ex_20.csv")


dfdelta<-data.frame(Ic=ICER_res_new[,5],Ie=ICER_res_new[,6])
dfdelta$Ic<- scale(dfdelta$Ic)
dfdelta$Ie<- scale(dfdelta$Ie)
dfdelta<-na.omit(dfdelta)

dist_ref<-as.data.frame(matrix(ncol = 1,nrow = length(dfdelta$Ic)))

for (i in 1:length(dfdelta$Ic)) {
  dist_temp=as.data.frame(matrix(ncol = 1,nrow = length(dfdelta$Ic)))
  for (j in 1:length(dfdelta$Ic)) {
    dist_temp[j,1]=sqrt((dfdelta[i,1]-dfdelta[j,1])^2+(dfdelta[i,2]-dfdelta[j,2])^2)
  }
  dist_ref[i,1]=mean(dist_temp$V1)
}
dist_ref<-cbind(seq(1,length(dist_ref$V1),1),dist_ref)
dist_ref<-arrange(dist_ref,V1)

ICER_res_new$`nivpfs-nivos-ipipfs-ipios`[dist_ref$`seq(1, length(dist_ref$V1), 1)`[1]]

summary(dfdelta)

aes_plot<-function(x){0}

6.5 fit
icer_6.5_6.5ys<-
  ggplot(dfdelta, aes(x = Ie, y =Ic)) +
                geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
                geom_point(aes(y=Ic[58] ,x=Ie[58]),shape=16,colour="red",size=4)+
                scale_fill_continuous()+
                # stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
                coord_cartesian(ylim=c(-3,3),xlim = c(-3,3))+
                theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
                         axis.title = element_text(size = 16),plot.title = element_text(size = 16),plot.subtitle = element_text(size = 14))+
                labs(title="ICER 6.5-year horizon",
                     subtitle = "6.5Ys-fit data",
                     x="Incremental QALYs(standardized)",
                     y="Incremental Cost(standardized)")+
                # geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
                stat_function(fun=aes_plot, geom="line",xlim = c(-3,3),size=1,colour="black",linetype="dashed")
icer_6.5_6.5ys

icer_20_6.5ys<-
  ggplot(dfdelta, aes(x = Ie, y =Ic)) +
  geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
  geom_point(aes(y=Ic[69] ,x=Ie[69]),shape=16,colour="red",size=4)+
  scale_fill_continuous()+
  # stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
  coord_cartesian(ylim=c(-3,3),xlim = c(-3,3))+
  theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
                                  axis.title = element_text(size = 16),plot.title = element_text(size = 16),plot.subtitle = element_text(size = 14))+
  labs(title="ICER 20-year horizon",
       subtitle = "6.5Ys-fit data",
       x="Incremental QALYs(standardized)",
       y="Incremental Cost(standardized)")+
  # geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
  stat_function(fun=aes_plot, geom="line",xlim = c(-3,3),size=1,colour="black",linetype="dashed")
icer_20_6.5ys

# 3 fit
icer_6.5_3ysfit<-
  ggplot(dfdelta, aes(x = Ie, y =Ic)) +
                geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
                geom_point(aes(y=Ic[30] ,x=Ie[30]),shape=16,colour="red",size=4)+
                scale_fill_continuous()+
                # stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
                coord_cartesian(ylim=c(-3,3),xlim = c(-3,3))+
  theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),plot.title = element_text(size = 16),plot.subtitle = element_text(size = 14))+
                labs(title="ICER 6.5-year horizon",
                     subtitle = "3Ys-fit data",
                     x="Incremental QALYs(standardized)",
                     y="Incremental Cost(standardized)")+
                # geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
                stat_function(fun=aes_plot, geom="line",xlim = c(-3,3),size=1,colour="black",linetype="dashed")
icer_6.5_3ysfit

icer_20_3ysfit<-
  ggplot(dfdelta, aes(x = Ie, y =Ic)) +
                geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
                geom_point(aes(y=Ic[39] ,x=Ie[39]),shape=16,colour="red",size=4)+
                scale_fill_continuous()+
                # stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
                coord_cartesian(ylim=c(-3,3),xlim = c(-3,3))+
  theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),plot.title = element_text(size = 16),plot.subtitle = element_text(size = 14))+
                labs(title="ICER 20-year horizon",
                     subtitle = "3Ys-fit data",
                     x="Incremental QALYs(standardized)",
                     y="Incremental Cost(standardized)")+
                # geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
                stat_function(fun=aes_plot, geom="line",xlim = c(-3,3),size=1,colour="black",linetype="dashed")
icer_20_3ysfit

# 3 extrapolate
icer_6.5_3ysextra<-
  ggplot(dfdelta, aes(x = Ie, y =Ic)) +
                geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
                geom_point(aes(y=Ic[22] ,x=Ie[22]),shape=16,colour="red",size=4)+
                scale_fill_continuous()+
                # stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
                coord_cartesian(ylim=c(-3,3),xlim = c(-3,3))+
  theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),plot.title = element_text(size = 16),plot.subtitle = element_text(size = 14))+
                labs(title="ICER 6.5-year horizon",
                     subtitle = "3Ys-extrapolate data",
                     x="Incremental QALYs(standardized)",
                     y="Incremental Cost(standardized)")+
                # geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
                stat_function(fun=aes_plot, geom="line",xlim = c(-3,3),size=1,colour="black",linetype="dashed")
icer_6.5_3ysextra

icer_20_3ysextra<-
  ggplot(dfdelta, aes(x = Ie, y =Ic)) +
                geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
                geom_point(aes(y=Ic[4] ,x=Ie[4]),shape=16,colour="red",size=4)+
                scale_fill_continuous()+
                # stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
                coord_cartesian(ylim=c(-3,3),xlim = c(-3,3))+
  theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),plot.title = element_text(size = 16),plot.subtitle = element_text(size = 14))+
                labs(title="ICER 20-year horizon",
                     subtitle = "3Ys-extrapolate data",
                     x="Incremental QALYs(standardized)",
                     y="Incremental Cost(standardized)")+
                # geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
                stat_function(fun=aes_plot, geom="line",xlim = c(-3,3),size=1,colour="black",linetype="dashed")
icer_20_3ysextra


icer_plot_combined<-grid.arrange(icer_6.5_6.5ys,icer_20_6.5ys,
                                 icer_6.5_3ysfit,icer_20_3ysfit,
                                 icer_6.5_3ysextra,icer_20_3ysextra,
                                 ncol=2, widths=c(18, 18))

ggsave("icer_plot_combined_scaled.png",plot=icer_plot_combined,width = 16,height = 12,dpi = 1200)
