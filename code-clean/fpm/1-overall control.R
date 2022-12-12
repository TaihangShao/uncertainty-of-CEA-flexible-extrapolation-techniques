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
# library("progress") 

####---------------------------------------------------------------------------------------------------------------------------------###
# Notes
#3-further-output.r has no use
# pm_ind, an indicator to indicate that whether param-mix models have as obvious over-fit,1 for no over

####---------------------------------------------------------------------------------------------------------------------------------###
#3ys
#
md<-c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","FP1","FP2","RCS","RP-hazard","RP-odds","RP-normal","GAM","param-mix","mix-cure")
theme_set(theme_light())  # GGplot theme
path="C:/Users/Administrator/Desktop/pe-flex-revise/PE-submission-final/new-codes/fpm"
setwd(path)  
source('2-run the model.R')  
####---------------------------------------------------------------------------------------------------------------------------------###
###          OS           ###
####---------------------------------------------------------------------------------------------------------------------------------###

source("4-OS-data-input.R")

####---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------#######---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------###

#NIV
ltHaz<-ltHaz0
ltHaz_out<-ltHaz0_out
sd_bc<-data.frame("recyrs"=niv3y$recyrs,"censrec"=niv3y$censrec)
sd_bc_out<-data.frame("recyrs"=niv5y$recyrs,"censrec"=niv5y$censrec)
df_cure<-data.frame(niv5y$recyrs,3,niv3y$censrec,niv3y$recyrs)
colnames(df_cure)<-c("Tru_surv","Follow_Up","Censor","Obs_surv")
pm_ind_niv_os<-2

output_niv_os<-flexmodel(ltHaz,ltHaz_out,sd_bc,sd_bc_out,df_cure,pm_ind_niv_os)

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
# # 
fit_niv3y<-survfit(Surv(niv3y$recyrs,niv3y$censrec)~1,data=niv3y)
data_fit_niv3y<-data.frame(time=fit_niv3y$time, surv=fit_niv3y$surv,lower=fit_niv3y$lower,upper=fit_niv3y$upper)

fit_niv5y<-survfit(Surv(niv5y$recyrs,niv5y$censrec)~1,data=niv5y)
data_fit_niv5y<-data.frame(time=fit_niv5y$time, surv=fit_niv5y$surv,lower=fit_niv5y$lower,upper=fit_niv5y$upper)
# # 
source("6-plot-1-hazard-3-surv.R")
# 
# ggsave("niv-os-hazard.png",plot=fighr,width = 18,height = 12,dpi = 800)
# ggsave("niv-os-surv-3y.png",plot=f_surv_mod1,width = 18,height = 12,dpi = 800)
# ggsave("niv-os-surv-6y.png",plot=f_surv_mod2,width = 18,height = 12,dpi = 800)
# ggsave("niv-os-surv-20y.png",plot=f_surv_mod3,width = 18,height = 12,dpi = 800)
# ggsave("niv_os_mse_1.png",plot=fig_mse_1,width = 18,height = 12,dpi = 800)
# ggsave("niv_os_mse_2.png",plot=fig_mse_2,width = 18,height = 12,dpi = 800)
# write.csv(MSE_final,file = "niv_os_mse_table.csv")
# write.csv(dfGOF,file = "niv_os_dfgof.csv")
# write.csv(dfSurv,file = "niv_os_dfsurv_20y.csv")
# 
# write.csv(dfHazEst,file = "AUC/dfhaz-3yex-nivos.csv")
# write.csv(ltHaz,file = "AUC/lthaz-3y-nivos.csv")

# ggsave("niv_os_bias_1.png",plot=fig_bias_1,width = 18,height = 12,dpi = 800)
# ggsave("niv_os_bias_2.png",plot=fig_bias_2,width = 18,height = 12,dpi = 800)
# write.csv(bias_final,file = "niv_os_bias_table.csv")

ggsave("niv_os_auc_1.png",plot=fig_auc_1,width = 18,height = 12,dpi = 800)
ggsave("niv_os_auc_2.png",plot=fig_auc_2,width = 18,height = 12,dpi = 800)
write.csv(auc_final,file = "niv_os_auc_table.csv")


####---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------#######---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------###

#IPI
ltHaz<-ltHaz1
ltHaz_out<-ltHaz1_out
sd_bc<-data.frame("recyrs"=ipi3y$recyrs,"censrec"=ipi3y$censrec)
sd_bc_out<-data.frame("recyrs"=ipi5y$recyrs,"censrec"=ipi5y$censrec)
df_cure<-data.frame(ipi5y$recyrs,3,ipi3y$censrec,ipi3y$recyrs)
colnames(df_cure)<-c("Tru_surv","Follow_Up","Censor","Obs_surv")
pm_ind_ipi_os<-2

output_ipi_os<-flexmodel(ltHaz,ltHaz_out,sd_bc,sd_bc_out,df_cure,pm_ind_ipi_os)

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
# 
fit_niv3y<-survfit(Surv(ipi3y$recyrs,ipi3y$censrec)~1,data=ipi3y)
data_fit_niv3y<-data.frame(time=fit_niv3y$time, surv=fit_niv3y$surv,lower=fit_niv3y$lower,upper=fit_niv3y$upper)

fit_niv5y<-survfit(Surv(ipi5y$recyrs,ipi5y$censrec)~1,data=ipi5y)
data_fit_niv5y<-data.frame(time=fit_niv5y$time, surv=fit_niv5y$surv,lower=fit_niv5y$lower,upper=fit_niv5y$upper)

source("6-plot-1-hazard-3-surv.R")
# 
# ggsave("ipi-os-hazard.png",plot=fighr,width = 18,height = 12,dpi = 800)
# ggsave("ipi-os-surv-3y.png",plot=f_surv_mod1,width = 18,height = 12,dpi = 800)
# ggsave("ipi-os-surv-6y.png",plot=f_surv_mod2,width = 18,height = 12,dpi = 800)
# ggsave("ipi-os-surv-20y.png",plot=f_surv_mod3,width = 18,height = 12,dpi = 800)
# ggsave("ipi_os_mse_1.png",plot=fig_mse_1,width = 18,height = 12,dpi = 800)
# ggsave("ipi_os_mse_2.png",plot=fig_mse_2,width = 18,height = 12,dpi = 800)
# write.csv(MSE_final,file = "ipi_os_mse_table.csv")
# # write.csv(dfGOF,file = "ipi_os_dfgof.csv")
# write.csv(dfSurv,file = "ipi_os_dfsurv_20y.csv")
# 
# write.csv(dfHazEst,file = "AUC/dfhaz-3yex-ipios.csv")
# write.csv(ltHaz,file = "AUC/lthaz-3y-ipios.csv")

# ggsave("ipi_os_bias_1.png",plot=fig_bias_1,width = 18,height = 12,dpi = 800)
# ggsave("ipi_os_bias_2.png",plot=fig_bias_2,width = 18,height = 12,dpi = 800)
# write.csv(bias_final,file = "ipi_os_bias_table.csv")

ggsave("ipi_os_auc_1.png",plot=fig_auc_1,width = 18,height = 12,dpi = 800)
ggsave("ipi_os_auc_2.png",plot=fig_auc_2,width = 18,height = 12,dpi = 800)
write.csv(auc_final,file = "ipi_os_auc_table.csv")

####---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------#######---------------------------------------------------------------------------------------------------------------------------------###
###          PFS           ###
####---------------------------------------------------------------------------------------------------------------------------------#######---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------###
# source("3-further-output.R")

####---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------#######---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------###

source("5-PFS-data-input.R")

#NIV
ltHaz<-ltHaz0
ltHaz_out<-ltHaz0_out
sd_bc<-data.frame("recyrs"=niv3y$recyrs,"censrec"=niv3y$censrec)
sd_bc_out<-data.frame("recyrs"=niv5y$recyrs,"censrec"=niv5y$censrec)
df_cure<-data.frame(niv5y$recyrs,3,niv3y$censrec,niv3y$recyrs)
colnames(df_cure)<-c("Tru_surv","Follow_Up","Censor","Obs_surv")
pm_ind_niv_pfs<-1

output_niv_pfs<-flexmodel(ltHaz,ltHaz_out,sd_bc,sd_bc_out,df_cure,pm_ind_niv_pfs)

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
#
#
source("6-plot-1-hazard-3-surv.R")

# ggsave("niv-pfs-hazard.png",plot=fighr,width = 18,height = 12,dpi = 800)
# ggsave("niv-pfs-surv-3y.png",plot=f_surv_mod1,width = 18,height = 12,dpi = 800)
# ggsave("niv-pfs-surv-6y.png",plot=f_surv_mod2,width = 18,height = 12,dpi = 800)
# ggsave("niv-pfs-surv-20y.png",plot=f_surv_mod3,width = 18,height = 12,dpi = 800)
# ggsave("niv_pfs_mse_1.png",plot=fig_mse_1,width = 18,height = 12,dpi = 800)
# ggsave("niv_pfs_mse_2.png",plot=fig_mse_2,width = 18,height = 12,dpi = 800)
# write.csv(MSE_final,file = "niv_pfs_mse_table.csv")
# write.csv(dfGOF,file = "niv_pfs_dfgof.csv")
# write.csv(dfSurv,file = "niv_pfs_dfsurv_20y.csv")
# 
# write.csv(dfHazEst,file = "AUC/dfhaz-3yex-nivpfs.csv")
# write.csv(ltHaz,file = "AUC/lthaz-3y-nivpfs.csv")

# ggsave("niv_pfs_bias_1.png",plot=fig_bias_1,width = 18,height = 12,dpi = 800)
# ggsave("niv_pfs_bias_2.png",plot=fig_bias_2,width = 18,height = 12,dpi = 800)
# write.csv(bias_final,file = "niv_pfs_bias_table.csv")

ggsave("niv_pfs_auc_1.png",plot=fig_auc_1,width = 18,height = 12,dpi = 800)
ggsave("niv_pfs_auc_2.png",plot=fig_auc_2,width = 18,height = 12,dpi = 800)
write.csv(auc_final,file = "niv_pfs_auc_table.csv")

####---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------#######---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------###

#IPI
ltHaz<-ltHaz1
ltHaz_out<-ltHaz1_out
sd_bc<-data.frame("recyrs"=ipi3y$recyrs,"censrec"=ipi3y$censrec)
sd_bc_out<-data.frame("recyrs"=ipi5y$recyrs,"censrec"=ipi5y$censrec)
df_cure<-data.frame(ipi5y$recyrs,3,ipi3y$censrec,ipi3y$recyrs)
colnames(df_cure)<-c("Tru_surv","Follow_Up","Censor","Obs_surv")
pm_ind_ipi_pfs<-1

output_ipi_pfs<-flexmodel(ltHaz,ltHaz_out,sd_bc,sd_bc_out,df_cure,pm_ind_ipi_pfs)

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

# 
fit_niv3y<-survfit(Surv(ipi3y$recyrs,ipi3y$censrec)~1,data=ipi3y)
data_fit_niv3y<-data.frame(time=fit_niv3y$time, surv=fit_niv3y$surv,lower=fit_niv3y$lower,upper=fit_niv3y$upper)

fit_niv5y<-survfit(Surv(ipi5y$recyrs,ipi5y$censrec)~1,data=ipi5y)
data_fit_niv5y<-data.frame(time=fit_niv5y$time, surv=fit_niv5y$surv,lower=fit_niv5y$lower,upper=fit_niv5y$upper)
#

source("6-plot-1-hazard-3-surv.R")

# ggsave("ipi-pfs-hazard.png",plot=fighr,width = 18,height = 12,dpi = 800)
# ggsave("ipi-pfs-surv-3y.png",plot=f_surv_mod1,width = 18,height = 12,dpi = 800)
# ggsave("ipi-pfs-surv-6y.png",plot=f_surv_mod2,width = 18,height = 12,dpi = 800)
# ggsave("ipi-pfs-surv-20y.png",plot=f_surv_mod3,width = 18,height = 12,dpi = 800)
# ggsave("ipi_pfs_mse_1.png",plot=fig_mse_1,width = 18,height = 12,dpi = 800)
# ggsave("ipi_pfs_mse_2.png",plot=fig_mse_2,width = 18,height = 12,dpi = 800)
# write.csv(MSE_final,file = "ipi_pfs_mse_table.csv")
# write.csv(dfGOF,file = "ipi_pfs_dfgof.csv")
# write.csv(dfSurv,file = "ipi_pfs_dfsurv_20y.csv")
# 
# write.csv(dfHazEst,file = "AUC/dfhaz-3yex-ipipfs.csv")
# write.csv(ltHaz,file = "AUC/lthaz-3y-ipipfs.csv")

ggsave("ipi_pfs_auc_1.png",plot=fig_auc_1,width = 18,height = 12,dpi = 800)
ggsave("ipi_pfs_auc_2.png",plot=fig_auc_2,width = 18,height = 12,dpi = 800)
write.csv(auc_final,file = "ipi_pfs_auc_table.csv")

####---------------------------------------------------------------------------------------------------------------------------------#######---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------#######---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------#######---------------------------------------------------------------------------------------------------------------------------------###
#6.5ys
####---------------------------------------------------------------------------------------------------------------------------------###
md<-c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","FP1","FP2","RCS","RP-hazard","RP-odds","RP-normal","GAM","param-mix","mix-cure")
theme_set(theme_light())  # GGplot theme
path="C:/Users/Administrator/Desktop/pe-flex-revise/PE-submission-final/new-codes/fpm"
setwd(path)  
source('8-run-model-6.5ys.R')  
####---------------------------------------------------------------------------------------------------------------------------------###
###          OS           ###
####---------------------------------------------------------------------------------------------------------------------------------###

source("4-OS-data-input.R")

####---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------###

#NIV
ltHaz<-ltHaz0
ltHaz_out<-ltHaz0_out
sd_bc<-data.frame("recyrs"=niv3y$recyrs,"censrec"=niv3y$censrec)
sd_bc_out<-data.frame("recyrs"=niv5y$recyrs,"censrec"=niv5y$censrec)
df_cure<-data.frame(niv5y$recyrs,6.5,niv5y$censrec,niv5y$recyrs)
colnames(df_cure)<-c("Tru_surv","Follow_Up","Censor","Obs_surv")
pm_ind_niv_os<-1

output_niv_os_6.5ys<-flexmodel_6.5ys(ltHaz,ltHaz_out,sd_bc,sd_bc_out,df_cure,pm_ind_niv_os)

############################-----------------------------------------------###
########  input  ###########-----------------------------------------------###
############################-----------------------------------------------###
output0<-output_niv_os_6.5ys
############################-----------------------------------------------###
############################-----------------------------------------------###

dfGOF<-output0[[1]]
dfGOF$LnL<-as.numeric(dfGOF$LnL)
dfGOF$Params<-as.numeric(dfGOF$Params)
dfGOF$AIC <- -2*dfGOF$LnL + 2*dfGOF$Params
ltHaz_out<-output0[[2]]
dfHazEst3<-output0[[3]]
dfSurv4<-output0[[4]]
dfSurv5<-output0[[5]]
# 
fit_niv5y<-survfit(Surv(niv5y$recyrs,niv5y$censrec)~1,data=niv5y)
data_fit_niv5y<-data.frame(time=fit_niv5y$time, surv=fit_niv5y$surv,lower=fit_niv5y$lower,upper=fit_niv5y$upper)

source("9-plot-1-hazard-3-surv_6.5ys.R")

# ggsave("6.5-niv-os-hazard.png",plot=fighr,width = 18,height = 12,dpi = 800)
# ggsave("6.5-niv-os-surv-3y.png",plot=f_surv_mod1,width = 18,height = 12,dpi = 800)
# ggsave("6.5-niv-os-surv-20y.png",plot=f_surv_mod3,width = 18,height = 12,dpi = 800)
# ggsave("6.5-niv_os_mse_1.png",plot=fig_mse_1,width = 18,height = 12,dpi = 800)
# write.csv(MSE,file = "6.5-niv_os_mse_table.csv")
# # write.csv(dfGOF,file = "6.5-niv_os_dfgof.csv")
# # write.csv(dfSurv4,file = "6.5-niv_os_dfsurv_20y.csv")
# 
# write.csv(ltHaz_out,file = "AUC/lthaz-6.5y-nivos.csv")

ggsave("6.5-niv_os_bias_1.png",plot=fig_bias_1,width = 18,height = 12,dpi = 800)
write.csv(bias,file = "6.5-niv_os_bias_table.csv")

ggsave("6.5-niv_os_auc_1.png",plot=fig_auc_1,width = 18,height = 12,dpi = 800)
write.csv(auc,file = "6.5-niv_os_auc_table.csv")


####---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------###

#IPI
ltHaz<-ltHaz1
ltHaz_out<-ltHaz1_out
sd_bc<-data.frame("recyrs"=ipi3y$recyrs,"censrec"=ipi3y$censrec)
sd_bc_out<-data.frame("recyrs"=ipi5y$recyrs,"censrec"=ipi5y$censrec)
df_cure<-data.frame(ipi5y$recyrs,6.5,ipi5y$censrec,ipi5y$recyrs)
colnames(df_cure)<-c("Tru_surv","Follow_Up","Censor","Obs_surv")
pm_ind_ipi_os<-2

output_ipi_os_6.5ys<-flexmodel_6.5ys(ltHaz,ltHaz_out,sd_bc,sd_bc_out,df_cure,pm_ind_ipi_os)

############################-----------------------------------------------###
########  input  ###########-----------------------------------------------###
############################-----------------------------------------------###
output0<-output_ipi_os_6.5ys
############################-----------------------------------------------###
############################-----------------------------------------------###

dfGOF<-output0[[1]]
dfGOF$LnL<-as.numeric(dfGOF$LnL)
dfGOF$Params<-as.numeric(dfGOF$Params)
dfGOF$AIC <- -2*dfGOF$LnL + 2*dfGOF$Params
ltHaz_out<-output0[[2]]
dfHazEst3<-output0[[3]]
dfSurv4<-output0[[4]]
dfSurv5<-output0[[5]]

fit_niv5y<-survfit(Surv(ipi5y$recyrs,ipi5y$censrec)~1,data=ipi5y)
data_fit_niv5y<-data.frame(time=fit_niv5y$time, surv=fit_niv5y$surv,lower=fit_niv5y$lower,upper=fit_niv5y$upper)

source("9-plot-1-hazard-3-surv_6.5ys.R")

# ggsave("6.5-ipi-os-hazard.png",plot=fighr,width = 18,height = 12,dpi = 800)
# ggsave("6.5-ipi-os-surv-3y.png",plot=f_surv_mod1,width = 18,height = 12,dpi = 800)
# ggsave("6.5-ipi-os-surv-20y.png",plot=f_surv_mod3,width = 18,height = 12,dpi = 800)
# ggsave("6.5-ipi_os_mse_1.png",plot=fig_mse_1,width = 18,height = 12,dpi = 800)
# write.csv(MSE,file = "6.5-ipi_os_mse_table.csv")
# # write.csv(dfGOF,file = "6.5-ipi_os_dfgof.csv")
# # write.csv(dfSurv4,file = "6.5-ipi_os_dfsurv_20y.csv")
# 
# write.csv(ltHaz_out,file = "AUC/lthaz-6.5y-ipios.csv")

ggsave("6.5-ipi_os_bias_1.png",plot=fig_bias_1,width = 18,height = 12,dpi = 800)
write.csv(bias,file = "6.5-ipi_os_bias_table.csv")

ggsave("6.5-ipi_os_auc_1.png",plot=fig_auc_1,width = 18,height = 12,dpi = 800)
write.csv(auc,file = "6.5-ipi_os_auc_table.csv")

####---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------#######---------------------------------------------------------------------------------------------------------------------------------###
###          PFS           ###
####---------------------------------------------------------------------------------------------------------------------------------#######---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------###
# source("3-further-output.R")

####---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------#######---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------###

source("5-PFS-data-input.R")

#NIV
ltHaz<-ltHaz0
ltHaz_out<-ltHaz0_out
sd_bc<-data.frame("recyrs"=niv3y$recyrs,"censrec"=niv3y$censrec)
sd_bc_out<-data.frame("recyrs"=niv5y$recyrs,"censrec"=niv5y$censrec)
df_cure<-data.frame(niv5y$recyrs,6.5,niv5y$censrec,niv5y$recyrs)
colnames(df_cure)<-c("Tru_surv","Follow_Up","Censor","Obs_surv")
pm_ind_niv_pfs<-2

output_niv_pfs_6.5ys<-flexmodel_6.5ys(ltHaz,ltHaz_out,sd_bc,sd_bc_out,df_cure,pm_ind_niv_pfs)

############################-----------------------------------------------###
########  input  ###########-----------------------------------------------###
############################-----------------------------------------------###
output0<-output_niv_pfs_6.5ys
############################-----------------------------------------------###
############################-----------------------------------------------###

dfGOF<-output0[[1]]
dfGOF$LnL<-as.numeric(dfGOF$LnL)
dfGOF$Params<-as.numeric(dfGOF$Params)
dfGOF$AIC <- -2*dfGOF$LnL + 2*dfGOF$Params
ltHaz_out<-output0[[2]]
dfHazEst3<-output0[[3]]
dfSurv4<-output0[[4]]
dfSurv5<-output0[[5]]

fit_niv5y<-survfit(Surv(niv5y$recyrs,niv5y$censrec)~1,data=niv5y)
data_fit_niv5y<-data.frame(time=fit_niv5y$time, surv=fit_niv5y$surv,lower=fit_niv5y$lower,upper=fit_niv5y$upper)


source("9-plot-1-hazard-3-surv_6.5ys.R")

# ggsave("6.5-niv-pfs-hazard.png",plot=fighr,width = 18,height = 12,dpi = 800)
# ggsave("6.5-niv-pfs-surv-3y.png",plot=f_surv_mod1,width = 18,height = 12,dpi = 800)
# ggsave("6.5-niv-pfs-surv-20y.png",plot=f_surv_mod3,width = 18,height = 12,dpi = 800)
# ggsave("6.5-niv_pfs_mse_1.png",plot=fig_mse_1,width = 18,height = 12,dpi = 800)
# write.csv(MSE,file = "6.5-niv_pfs_mse_table.csv")
# # write.csv(dfGOF,file = "6.5-niv_pfs_dfgof.csv")
# # write.csv(dfSurv4,file = "6.5-niv_pfs_dfsurv_20y.csv")
# 
# write.csv(ltHaz_out,file = "AUC/lthaz-6.5y-nivpfs.csv")

ggsave("6.5-niv_pfs_bias_1.png",plot=fig_bias_1,width = 18,height = 12,dpi = 800)
write.csv(bias,file = "6.5-niv_pfs_bias_table.csv")

ggsave("6.5-niv_pfs_auc_1.png",plot=fig_auc_1,width = 18,height = 12,dpi = 800)
write.csv(auc,file = "6.5-niv_pfs_auc_table.csv")


####---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------###

#IPI
ltHaz<-ltHaz1
ltHaz_out<-ltHaz1_out
sd_bc<-data.frame("recyrs"=ipi3y$recyrs,"censrec"=ipi3y$censrec)
sd_bc_out<-data.frame("recyrs"=ipi5y$recyrs,"censrec"=ipi5y$censrec)
df_cure<-data.frame(ipi5y$recyrs,6.5,ipi5y$censrec,ipi5y$recyrs)
colnames(df_cure)<-c("Tru_surv","Follow_Up","Censor","Obs_surv")
pm_ind_ipi_pfs<-1

output_ipi_pfs_6.5ys<-flexmodel_6.5ys(ltHaz,ltHaz_out,sd_bc,sd_bc_out,df_cure,pm_ind_ipi_pfs)

############################-----------------------------------------------###
########  input  ###########-----------------------------------------------###
############################-----------------------------------------------###
output0<-output_ipi_pfs_6.5ys
############################-----------------------------------------------###
############################-----------------------------------------------###

dfGOF<-output0[[1]]
dfGOF$LnL<-as.numeric(dfGOF$LnL)
dfGOF$Params<-as.numeric(dfGOF$Params)
dfGOF$AIC <- -2*dfGOF$LnL + 2*dfGOF$Params
ltHaz_out<-output0[[2]]
dfHazEst3<-output0[[3]]
dfSurv4<-output0[[4]]
dfSurv5<-output0[[5]]



fit_niv5y<-survfit(Surv(ipi5y$recyrs,ipi5y$censrec)~1,data=ipi5y)
data_fit_niv5y<-data.frame(time=fit_niv5y$time, surv=fit_niv5y$surv,lower=fit_niv5y$lower,upper=fit_niv5y$upper)


source("9-plot-1-hazard-3-surv_6.5ys.R")

# ggsave("6.5-ipi-pfs-hazard.png",plot=fighr,width = 18,height = 12,dpi = 800)
# ggsave("6.5-ipi-pfs-surv-3y.png",plot=f_surv_mod1,width = 18,height = 12,dpi = 800)
# ggsave("6.5-ipi-pfs-surv-20y.png",plot=f_surv_mod3,width = 18,height = 12,dpi = 800)
# ggsave("6.5-ipi_pfs_mse_1.png",plot=fig_mse_1,width = 18,height = 12,dpi = 800)
# write.csv(MSE,file = "6.5-ipi_pfs_mse_table.csv")
# # write.csv(dfGOF,file = "6.5-ipi_pfs_dfgof.csv")
# # write.csv(dfSurv4,file = "6.5-ipi_pfs_dfsurv_20y.csv")
# 
# write.csv(ltHaz_out,file = "AUC/lthaz-6.5y-ipipfs.csv")


ggsave("6.5-ipi_pfs_bias_1.png",plot=fig_bias_1,width = 18,height = 12,dpi = 800)
write.csv(bias,file = "6.5-ipi_pfs_bias_table.csv")

ggsave("6.5-ipi_pfs_auc_1.png",plot=fig_auc_1,width = 18,height = 12,dpi = 800)
write.csv(auc,file = "6.5-ipi_pfs_auc_table.csv")

####---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------###
####---------------------------------------------------------------------------------------------------------------------------------###








