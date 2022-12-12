library("tidyverse")   # Mainly for plotting
library("discSurv")    # Create life tables
library("flexsurv")    # RPs (also loads survival) and BC case-study
library("lme4")        # RE components
library("mgcv")        # GAM
library("KFAS")        # DSM
library("gridExtra")   # Plotting
library("zoo")         # Aid in time-series analyses
library("ggplot2")     # Mainly for plotting
library("survHE")      # Plotting
library("survminer")   # KM curve
library("here")        # Here
library("rstan")       # stan file  
library("parallel")    # More CPU cores to run the code
library("cuRe")        # old cure model
library("flexsurvcure")# new cure model
theme_set(theme_light())  # GGplot theme

df<-read.csv("data.csv")

f_surv_mod2= ggplot(data = df,aes(x=Num,y=Value,group=Model,shape=Model,colour=Model)) +
  geom_point(size=5)+
  scale_color_discrete(name="Model")+
  scale_shape_manual(values = seq(1,16,1))+
  # scale_size_manual(values = 2)+
  # expand_limits(x=c(1,3)) +
  # # # facet_wrap(~Model,nrow=4)+
  scale_x_continuous(labels=NULL) +
  facet_wrap(~ Group,nrow = 1) + 
  ylab("MSE") +
  xlab("Three Groups of Models (3-year data fit; 3-year data extrapolate; 6.5-year data fit)") +
  labs(title = "All Models Included")+
  guides(color = guide_legend(nrow = 2))  +
  theme(legend.position = "bottom",axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),plot.title = element_text(size = 16),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12),strip.text = element_text(size = 12)) 
f_surv_mod2


df2<-read.csv("data2.csv")

f_surv_mod3= ggplot(data = df2,aes(x=Num,y=Value,group=Model,shape=Rank,colour=Rank)) +
  geom_point(size=5)+
  geom_text(data = df2, aes(x=Num2,y=Value2,label=Label),size=3.5)+
  # geom_label(aes(label=label,fill=label),
  #            label.r = unit(0,'mm'),
  #            label.size = NA)+
  scale_color_discrete(name="Rank")+
  scale_shape_manual(values = c(15,16,17,18,4))+
  # scale_size_manual(values = 2)+
  expand_limits(x=c(1,4)) +
  # # facet_wrap(~Model,nrow=4)+
  scale_x_continuous(labels=NULL,breaks = c(seq(from=1, to=4,by = 1)))+
  facet_wrap(~ Group,nrow = 1) + 
  ylab("MSE") +
  xlab("Three Groups of Models (3-year data fit; 3-year data extrapolate; 6.5-year data fit)") +
  labs(title = "Top Five Models Included")+
  guides(color = guide_legend(nrow = 2))  +
  theme(legend.position = "bottom",axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),plot.title = element_text(size = 16),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12),strip.text = element_text(size = 12)) 
f_surv_mod3


df3<-read.csv("data3.csv")

f_surv_mod4= ggplot(data = df3,aes(x=Num,y=Value,group=Model,shape=Model,colour=Model)) +
  geom_point(size=5)+
  scale_color_discrete(name="Model")+
  scale_shape_manual(values = c(1,5))+
  # scale_size_manual(values = 2)+
  # expand_limits(y=c(0,1),x=c(0,6.5)) + 
  # # facet_wrap(~Model,nrow=4)+
  scale_x_continuous(labels=NULL) +
  facet_wrap(~ Group,nrow = 1) + 
  ylab("MSE") +
  xlab("Three Groups of Models (3-year data fit; 3-year data extrapolate; 6.5-year data fit)") +
  labs(title = "Standard parametric models vs. Flexible techniques")+
  guides(color = guide_legend(nrow = 2))  +
  theme(legend.position = "bottom",axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),plot.title = element_text(size = 16),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12),strip.text = element_text(size = 12)) 
f_surv_mod4


# grid.arrange(f_surv_mod2,
#              f_surv_mod3,
#              f_surv_mod4,
#              ncol=3,heights=12,
#              widths=c(8,8,8))
# 
# fig<-grid.arrange(f_surv_mod2,
#                   f_surv_mod4,
#              f_surv_mod3,
#              nrow=2,
#              ncol=2,heights=c(12,12),
#              widths=c(8,8))

fig<-grid.arrange(arrangeGrob(f_surv_mod2, f_surv_mod4,ncol = 2),f_surv_mod3,
                  nrow=2)

# grid.arrange(f_surv_mod2,
#              f_surv_mod4,
#              f_surv_mod3,
#              ncol=2,nrow=2,
#              layout_matrix=rbind(c(1,2),c(3,3)))

ggsave("fig.png",plot=fig,width = 24,height = 18,dpi = 450)






