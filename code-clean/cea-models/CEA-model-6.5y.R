# library("hesim")
library("flexsurv")
library("ggplot2")
library("data.table")
library("tidyverse")
library("tidyr")
library("tidygraph")
library("gridExtra")

param_origin<-read.csv("param.csv")
param<-param_origin[,2:9]

# cost input
c_niv_drug<-16002/42*30
c_niv_admin<-204.48/21*30

c_nivipi_drug<-73521/42*30
c_nivipi_admin<-219.79/21*30

c_ipi_drug<-56946/2/21*30
c_ipi_admin<-497.93/21*30

c_che_drug<-6377/2/21*30
c_che_admin<-497.93/21*30

nivipi_ae_f12<-0.31
nivipi_ae_d12<-0.35
nivipi_ae_r12<-0.36
nivipi_ae_n12<-0.24
nivipi_ae_h12<-0.15

nivipi_ae_f34<-0.04
nivipi_ae_d34<-0.09
nivipi_ae_r34<-0.05
nivipi_ae_n34<-0.02
nivipi_ae_h34<-0

ipi_ae_f12<-0.27
ipi_ae_d12<-0.27
ipi_ae_r12<-0.31
ipi_ae_n12<-0.15

ipi_ae_f34<-0.01
ipi_ae_d34<-0.06
ipi_ae_r34<-0.02
ipi_ae_n34<-0.01

niv_ae_f12<-0.24
niv_ae_d12<-0.11
niv_ae_r12<-0.16
niv_ae_n12<-0.09

niv_ae_f34<-0.01
niv_ae_d34<-0
niv_ae_r34<-0
niv_ae_n34<-0

che_ae_f12<-0.3
che_ae_d12<-0.13
che_ae_n12<-0.35

che_ae_f34<-0.04
che_ae_d34<-0.02
che_ae_n34<-0.02

u_ae_f12<-0.11
u_ae_d12<-0.09
u_ae_r12<-0.06
u_ae_n12<-0.1
u_ae_h12<-0

u_ae_f34<-0.11
u_ae_d34<-0.17
u_ae_r34<-0.13
u_ae_n34<-0.13
u_ae_h34<-0

c_ae_f12<-0
c_ae_d12<-8.64
c_ae_r12<-9.4
c_ae_n12<-311.16
c_ae_h12<-20

c_ae_f34<-0
c_ae_d34<-942.52
c_ae_r34<-399.26
c_ae_n34<-365.22
c_ae_h34<-20

u_base<-0.8
u_pd<-0.52

c_eol<-16998.42
c_bsc_drug<-152.01/21*30
c_bsc_admin<-497.93/21*30
p_bsc_nivipi<-0.46
p_bsc_ipi<-0.76

discount<-0.03
WTP<-64000
# cost in model

pfs_cost_ipi_4in<-c_ipi_admin+c_ipi_drug
pfs_cost_nivipi_4in<-c_nivipi_admin+c_nivipi_drug
pfs_cost_ipi_4out<-0
pfs_cost_nivipi_4out<-(c_niv_admin+c_niv_drug)*1.5
pfs_cost_nivipi_2y_out<-c_che_admin+c_che_drug
pd_cost_ipi<-(c_niv_admin+c_niv_drug)*1.5*p_bsc_ipi+(c_bsc_admin+c_bsc_drug)*(1-p_bsc_ipi)
pd_cost_nivipi <- (c_che_admin+c_che_drug)*p_bsc_nivipi+(c_bsc_admin+c_bsc_drug)*(1-p_bsc_nivipi)

# AE
## AE cost
c_ae12_pfs_nivipi<-nivipi_ae_f12*c_ae_f12+nivipi_ae_d12*c_ae_d12+nivipi_ae_r12*c_ae_r12+nivipi_ae_n12*c_ae_n12+nivipi_ae_h12*c_ae_h12
c_ae12_pd_nivipi<-che_ae_f12*c_ae_f12+che_ae_d12*c_ae_d12+che_ae_n12*c_ae_n12
c_ae34_pfs_nivipi<-nivipi_ae_f34*c_ae_f34+nivipi_ae_d34*c_ae_d34+nivipi_ae_r34*c_ae_r34+nivipi_ae_n34*c_ae_n34+nivipi_ae_h34*c_ae_h34
c_ae34_pd_nivipi<-che_ae_f34*c_ae_f34+che_ae_d34*c_ae_d34+che_ae_n34*c_ae_n34

c_ae12_pfs_ipi<-ipi_ae_f12*c_ae_f12+ipi_ae_d12*c_ae_d12+ipi_ae_r12*c_ae_r12+ipi_ae_n12*c_ae_n12
c_ae12_pd_ipi<-niv_ae_f12*c_ae_f12+niv_ae_d12*c_ae_d12+niv_ae_n12*c_ae_n12+niv_ae_r12*c_ae_r12
c_ae34_pfs_ipi<-ipi_ae_f34*c_ae_f34+ipi_ae_d34*c_ae_d34+ipi_ae_r34*c_ae_r34+ipi_ae_n34*c_ae_n34
c_ae34_pd_ipi<-niv_ae_f34*c_ae_f34+niv_ae_d34*c_ae_d34+niv_ae_n34*c_ae_n34+niv_ae_r34*c_ae_r34

## AE disutility
disu_ae12_pfs_nivipi<-nivipi_ae_f12*u_ae_f12+nivipi_ae_d12*u_ae_d12+nivipi_ae_r12*u_ae_r12+nivipi_ae_n12*u_ae_n12+nivipi_ae_h12*u_ae_h12
disu_ae12_pd_nivipi<-che_ae_f12*u_ae_f12+che_ae_d12*u_ae_d12+che_ae_n12*u_ae_n12
disu_ae34_pfs_nivipi<-nivipi_ae_f34*u_ae_f34+nivipi_ae_d34*u_ae_d34+nivipi_ae_r34*u_ae_r34+nivipi_ae_n34*u_ae_n34+nivipi_ae_h34*u_ae_h34
disu_ae34_pd_nivipi<-che_ae_f34*u_ae_f34+che_ae_d34*u_ae_d34+che_ae_n34*u_ae_n34
  
disu_ae12_pfs_ipi<-ipi_ae_f12*u_ae_f12+ipi_ae_d12*u_ae_d12+ipi_ae_r12*u_ae_r12+ipi_ae_n12*u_ae_n12
disu_ae12_pd_ipi<-niv_ae_f12*u_ae_f12+niv_ae_d12*u_ae_d12+niv_ae_n12*u_ae_n12+niv_ae_r12*u_ae_r12
disu_ae34_pfs_ipi<-ipi_ae_f34*u_ae_f34+ipi_ae_d34*u_ae_d34+ipi_ae_r34*u_ae_r34+ipi_ae_n34*u_ae_n34
disu_ae34_pd_ipi<-niv_ae_f34*u_ae_f34+niv_ae_d34*u_ae_d34+niv_ae_n34*u_ae_n34+niv_ae_r34*u_ae_r34  



# simulation
##############################
# load data 6.5ys best fit
##############################
# setwd("C:/Users/Administrator/Desktop/pe-flex-revise/PE-submission-final/new-codes/cea-models/6.5ys-fit")
# 
# niv_pfs<-read.csv("6.5-niv_pfs_dfsurv_20y.csv")
# niv_os<-read.csv("6.5-niv_os_dfsurv_20y.csv")
# ipi_pfs<-read.csv("6.5-ipi_pfs_dfsurv_20y.csv")
# ipi_os<-read.csv("6.5-ipi_os_dfsurv_20y.csv")
# 
# niv_os<-data.frame(niv_os$Time,niv_os$mix.cure,niv_os$GAM,niv_os$RCS)
# niv_pfs<-data.frame(niv_pfs$Time,niv_pfs$RP.normal,niv_pfs$RP.odds,niv_pfs$RP.hazard)
# ipi_os<-data.frame(ipi_os$Time,ipi_os$RP.hazard,ipi_os$RP.normal,ipi_os$RP.odds)
# ipi_pfs<-data.frame(ipi_pfs$Time,ipi_pfs$RP.hazard,ipi_pfs$gompertz,ipi_pfs$GAM)
# 
# #A little adjust for survival rate (in 95%CI), so that the PSM model can be built
# niv_os[2,2:4]<-niv_os[2,2:4]+0.01
# ipi_pfs[2,2]<-ipi_pfs[2,2]-0.01
# 
# best_model<-as.data.frame(matrix(ncol = 3,nrow = 4))
# best_model[1,]<-c("mixcure","GAM","RCS")
# best_model[2,]<-c("RP_normal","RP_odds","RP_hazard")
# best_model[3,]<-c("RP_hazard","RP_normal","RP_odds")
# best_model[4,]<-c("RP.hazard","gompertz","GAM")


# ##############################
# ## load data 3ys best fit
# ##############################
# setwd("C:/Users/Administrator/Desktop/pe-flex-revise/PE-submission-final/new-codes/cea-models/3ys")
# 
# niv_pfs<-read.csv("niv_pfs_dfsurv_20y.csv")
# niv_os<-read.csv("niv_os_dfsurv_20y.csv")
# ipi_pfs<-read.csv("ipi_pfs_dfsurv_20y.csv")
# ipi_os<-read.csv("ipi_os_dfsurv_20y.csv")
# 
# niv_os<-data.frame(niv_os$Time,niv_os$gompertz,niv_os$GAM,niv_os$FP2)
# niv_pfs<-data.frame(niv_pfs$Time,niv_pfs$RP.normal,niv_pfs$GAM,niv_pfs$RCS)
# ipi_os<-data.frame(ipi_os$Time,ipi_os$RP.odds,ipi_os$RP.hazard,ipi_os$RP.normal)
# ipi_pfs<-data.frame(ipi_pfs$Time,ipi_pfs$gompertz,ipi_pfs$RP.hazard,ipi_pfs$FP2)
# 
# #A little adjust for survival rate (in 95%CI), so that the PSM model can be built
# niv_os[2,2:3]<-niv_os[2,2:3]+0.005
# ipi_pfs[2,3:4]<-ipi_pfs[2,3:4]-0.01
# 
# best_model<-as.data.frame(matrix(ncol = 3,nrow = 4))
# best_model[1,]<-c("gompertz","GAM","FP2")
# best_model[2,]<-c("RP_normal","GAM","RCS")
# best_model[3,]<-c("RP_odds","RP_hazard","RP_normal")
# best_model[4,]<-c("gompertz","RP_hazard","FP2")
# 
###############################
# ## load data 3ys best extra
###############################
setwd("C:/Users/Administrator/Desktop/pe-flex-revise/PE-submission-final/new-codes/cea-models/3ys")
niv_pfs<-read.csv("niv_pfs_dfsurv_20y.csv")
niv_os<-read.csv("niv_os_dfsurv_20y.csv")
ipi_pfs<-read.csv("ipi_pfs_dfsurv_20y.csv")
ipi_os<-read.csv("ipi_os_dfsurv_20y.csv")

niv_os<-data.frame(niv_os$Time,niv_os$mix.cure,niv_os$gompertz,niv_os$FP1)
niv_pfs<-data.frame(niv_pfs$Time,niv_pfs$GAM,niv_pfs$gompertz,niv_pfs$RCS)
ipi_os<-data.frame(ipi_os$Time,ipi_os$FP2,ipi_os$GAM,ipi_os$RCS)
ipi_pfs<-data.frame(ipi_pfs$Time,ipi_pfs$FP2,ipi_pfs$gompertz,ipi_pfs$RP.hazard)

#A little adjust for survival rate (in 95%CI), so that the PSM model can be built
niv_os[2,2:4]<-niv_os[2,2:4]+0.005
ipi_os[2,2:4]<-ipi_os[2,2:4]+0.01
ipi_pfs[2,2:4]<-ipi_pfs[2,2:4]-0.01


best_model<-as.data.frame(matrix(ncol = 3,nrow = 4))
best_model[1,]<-c("mixcure","gompertz","FP1")
best_model[2,]<-c("GAM","gompertz","RCS")
best_model[3,]<-c("FP2","GAM","RCS")
best_model[4,]<-c("FP2","gompertz","RP_hazard")



###############################
# ## rename colnames
###############################
colnames(niv_os)<-c("Time","Best_model_1","Best_model_2","Best_model_3")
colnames(niv_pfs)<-c("Time","Best_model_1","Best_model_2","Best_model_3")
colnames(ipi_os)<-c("Time","Best_model_1","Best_model_2","Best_model_3")
colnames(ipi_pfs)<-c("Time","Best_model_1","Best_model_2","Best_model_3")

##########################
## select Simulate time ##
##########################
##simulate 6.5y LE   cycle=78
##simulate 20y LE   cycle=240


cycle<-240


cir_time<-c(niv_os$Time[1:cycle],niv_os$Time[cycle]+niv_os$Time[2]-niv_os$Time[1])
niv_pfs<-niv_pfs[1:cycle,]
niv_os<-niv_os[1:cycle,]
ipi_pfs<-ipi_pfs[1:cycle,]
ipi_os<-ipi_os[1:cycle,]


#####################
## start PSM model ##
#####################
# run the model
#####################
## res
ICER_res<-as.data.frame(array(dim=c(3^4,6)))
colnames(ICER_res)<-c("Combination","ICER","ICER_PFS","INB","Cost","QALY")
a<-1
b<-1
c<-1
d<-1
index<-1

## for cycle 3^3 ICER
# PSM
for (a in 1:3) {
 
  # half cycle correction
  pfs_niv<-niv_pfs[,a+1]
  pfs_niv<-c(1,pfs_niv)
  for (i in 1:cycle) {
    pfs_niv[i]<-(pfs_niv[i]+pfs_niv[i+1])/2
  }
  
  for (b in 1:3) {
    # half cycle correction
    os_niv<-niv_os[,b+1]
    os_niv<-c(1,os_niv)
    for (i in 1:cycle) {
      os_niv[i]<-(os_niv[i]+os_niv[i+1])/2
    }
    
    for (c in 1:3) {
      # half cycle correction
      pfs_ipi<-ipi_pfs[,c+1]
      pfs_ipi<-c(1,pfs_ipi)
      for (i in 1:cycle) {
        pfs_ipi[i]<-(pfs_ipi[i]+pfs_ipi[i+1])/2
      }
      
      for (d in 1:3) {
        # half cycle correction
        os_ipi<-ipi_os[,d+1]
        os_ipi<-c(1,os_ipi)
        for (i in 1:cycle) {
          os_ipi[i]<-(os_ipi[i]+os_ipi[i+1])/2
        }
        
        # MARKOV data
        sim_niv<-data.frame(pfs_niv,os_niv-pfs_niv,1-os_niv)
        colnames(sim_niv)<-c("PFS","PD","Death")
        sim_ipi<-data.frame(pfs_ipi,os_ipi-pfs_ipi,1-os_ipi)
        colnames(sim_ipi)<-c("PFS","PD","Death")
        ## process data
        indic1<-0
        indic2<-0
        for (i in 1:(cycle+1)) {
          if (sim_niv$PD[i]<0) {indic1 = 1}
          if (indic1==1) {break}
        }
        for (i in 1:(cycle+1)) {
          if (sim_ipi$PD[i]<0) {indic2 = 1}
          if (indic2==1) {break}
        }
        ## if data is error, break
        if (indic1 ==1 | indic2 ==1){
          ICER_res$Combination[index]<-"error"
          ICER_res$ICER[index]<-"NA"
          ICER_res$ICER_PFS[index]<-"NA"
          index=index+1
          }
        else{
          #circulation data
          cir_temp<-as.data.frame(array(dim = c((cycle+1),18)))
          colnames(cir_temp)<-c("Cycle","Time","NIV-Cost","NIV-Cost_dis","NIV-Cost_pfs","NIV-Cost_pfs_dis","NIV-Utility","NIV-Utility_dis","NIV-Utility_pfs","NIV-Utility_pfs_dis"
                                ,"IPI-Cost","IPI-Cost_dis","IPI-Cost_pfs","IPI-Cost_pfs_dis","IPI-Utility","IPI-Utility_dis","IPI-Utility_pfs","IPI-Utility_pfs_dis")
          
          cir_temp$Cycle<-seq(1,(cycle+1),1)
          cir_temp$Time<-as.integer(cir_time) +1
          # cycle 1
          j<-1
            cir_temp$`NIV-Cost`[j]<-pfs_cost_nivipi_4in*sim_niv$PFS[j]+pd_cost_nivipi*sim_niv$PD[j]+c_eol*sim_niv$Death[j]
            cir_temp$`NIV-Cost_dis`[j]<-cir_temp$`NIV-Cost`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Cost_pfs`[j]<-pfs_cost_nivipi_4in*sim_niv$PFS[j]
            cir_temp$`NIV-Cost_pfs_dis`[j]<-cir_temp$`NIV-Cost_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Utility`[j]<-u_base*sim_niv$PFS[j]+u_pd*sim_niv$PD[j]
            cir_temp$`NIV-Utility_dis`[j]<-cir_temp$`NIV-Utility`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Utility_pfs`[j]<-u_base*sim_niv$PFS[j]
            cir_temp$`NIV-Utility_pfs_dis`[j]<-cir_temp$`NIV-Utility_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            
            cir_temp$`IPI-Cost`[j]<-pfs_cost_ipi_4in*sim_ipi$PFS[j]+pd_cost_ipi*sim_ipi$PD[j]+c_eol*sim_ipi$Death[j]
            cir_temp$`IPI-Cost_dis`[j]<-cir_temp$`IPI-Cost`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Cost_pfs`[j]<-pfs_cost_ipi_4in*sim_ipi$PFS[j]
            cir_temp$`IPI-Cost_pfs_dis`[j]<-cir_temp$`IPI-Cost_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Utility`[j]<-u_base*sim_ipi$PFS[j]+u_pd*sim_ipi$PD[j]
            cir_temp$`IPI-Utility_dis`[j]<-cir_temp$`IPI-Utility`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Utility_pfs`[j]<-u_base*sim_ipi$PFS[j]
            cir_temp$`IPI-Utility_pfs_dis`[j]<-cir_temp$`IPI-Utility_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
          # cycle 2-4 immunotherapy first 
          for (j in 2:3) {
            cir_temp$`NIV-Cost`[j]<-pfs_cost_nivipi_4in*sim_niv$PFS[j]+pd_cost_nivipi*sim_niv$PD[j]+c_eol*(sim_niv$Death[j]-sim_niv$Death[j-1])
            cir_temp$`NIV-Cost_dis`[j]<-cir_temp$`NIV-Cost`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Cost_pfs`[j]<-pfs_cost_nivipi_4in*sim_niv$PFS[j]
            cir_temp$`NIV-Cost_pfs_dis`[j]<-cir_temp$`NIV-Cost_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Utility`[j]<-u_base*sim_niv$PFS[j]+u_pd*sim_niv$PD[j]
            cir_temp$`NIV-Utility_dis`[j]<-cir_temp$`NIV-Utility`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Utility_pfs`[j]<-u_base*sim_niv$PFS[j]
            cir_temp$`NIV-Utility_pfs_dis`[j]<-cir_temp$`NIV-Utility_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            
            cir_temp$`IPI-Cost`[j]<-pfs_cost_ipi_4in*sim_ipi$PFS[j]+pd_cost_ipi*sim_ipi$PD[j]+c_eol*(sim_niv$Death[j]-sim_niv$Death[j-1])
            cir_temp$`IPI-Cost_dis`[j]<-cir_temp$`IPI-Cost`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Cost_pfs`[j]<-pfs_cost_ipi_4in*sim_ipi$PFS[j]
            cir_temp$`IPI-Cost_pfs_dis`[j]<-cir_temp$`IPI-Cost_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Utility`[j]<-u_base*sim_ipi$PFS[j]+u_pd*sim_ipi$PD[j]
            cir_temp$`IPI-Utility_dis`[j]<-cir_temp$`IPI-Utility`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Utility_pfs`[j]<-u_base*sim_ipi$PFS[j]
            cir_temp$`IPI-Utility_pfs_dis`[j]<-cir_temp$`IPI-Utility_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
           
          }
          # cycle 2-4 immunotherapy up to 2 years
          for (j in 4:24) {
            cir_temp$`NIV-Cost`[j]<-pfs_cost_nivipi_4out*sim_niv$PFS[j]+pd_cost_nivipi*sim_niv$PD[j]+c_eol*(sim_niv$Death[j]-sim_niv$Death[j-1])
            cir_temp$`NIV-Cost_dis`[j]<-cir_temp$`NIV-Cost`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Cost_pfs`[j]<-pfs_cost_nivipi_4out*sim_niv$PFS[j]
            cir_temp$`NIV-Cost_pfs_dis`[j]<-cir_temp$`NIV-Cost_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Utility`[j]<-u_base*sim_niv$PFS[j]+u_pd*sim_niv$PD[j]
            cir_temp$`NIV-Utility_dis`[j]<-cir_temp$`NIV-Utility`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Utility_pfs`[j]<-u_base*sim_niv$PFS[j]
            cir_temp$`NIV-Utility_pfs_dis`[j]<-cir_temp$`NIV-Utility_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            
            cir_temp$`IPI-Cost`[j]<-pfs_cost_ipi_4out*sim_ipi$PFS[j]+pd_cost_ipi*sim_ipi$PD[j]+c_eol*(sim_niv$Death[j]-sim_niv$Death[j-1])
            cir_temp$`IPI-Cost_dis`[j]<-cir_temp$`IPI-Cost`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Cost_pfs`[j]<-pfs_cost_ipi_4out*sim_ipi$PFS[j]
            cir_temp$`IPI-Cost_pfs_dis`[j]<-cir_temp$`IPI-Cost_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Utility`[j]<-u_base*sim_ipi$PFS[j]+u_pd*sim_ipi$PD[j]
            cir_temp$`IPI-Utility_dis`[j]<-cir_temp$`IPI-Utility`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Utility_pfs`[j]<-u_base*sim_ipi$PFS[j]
            cir_temp$`IPI-Utility_pfs_dis`[j]<-cir_temp$`IPI-Utility_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
          }
          # cycle 35-87 pd therapy
          for (j in 25:60) {
            cir_temp$`NIV-Cost`[j]<-pfs_cost_nivipi_2y_out*sim_niv$PFS[j]+pd_cost_nivipi*sim_niv$PD[j]+c_eol*(sim_niv$Death[j]-sim_niv$Death[j-1])
            cir_temp$`NIV-Cost_dis`[j]<-cir_temp$`NIV-Cost`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Cost_pfs`[j]<-pd_cost_nivipi*sim_niv$PFS[j]
            cir_temp$`NIV-Cost_pfs_dis`[j]<-cir_temp$`NIV-Cost_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Utility`[j]<-u_base*sim_niv$PFS[j]+u_pd*sim_niv$PD[j]
            cir_temp$`NIV-Utility_dis`[j]<-cir_temp$`NIV-Utility`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Utility_pfs`[j]<-u_base*sim_niv$PFS[j]
            cir_temp$`NIV-Utility_pfs_dis`[j]<-cir_temp$`NIV-Utility_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            
            cir_temp$`IPI-Cost`[j]<-pfs_cost_ipi_4out*sim_ipi$PFS[j]+pd_cost_ipi*sim_ipi$PD[j]+c_eol*(sim_niv$Death[j]-sim_niv$Death[j-1])
            cir_temp$`IPI-Cost_dis`[j]<-cir_temp$`IPI-Cost`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Cost_pfs`[j]<-pd_cost_ipi*sim_ipi$PFS[j]
            cir_temp$`IPI-Cost_pfs_dis`[j]<-cir_temp$`IPI-Cost_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Utility`[j]<-u_base*sim_ipi$PFS[j]+u_pd*sim_ipi$PD[j]
            cir_temp$`IPI-Utility_dis`[j]<-cir_temp$`IPI-Utility`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Utility_pfs`[j]<-u_base*sim_ipi$PFS[j]
            cir_temp$`IPI-Utility_pfs_dis`[j]<-cir_temp$`IPI-Utility_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
          }
          # cycle 87 to 349 pfs more than 5 years, cost of pfs equals to 0: we assumed that people in pfs has been cured
          for (j in 61:(cycle+1)) {
            cir_temp$`NIV-Cost`[j]<-0*sim_niv$PFS[j]+pd_cost_nivipi*sim_niv$PD[j]+c_eol*(sim_niv$Death[j]-sim_niv$Death[j-1])
            cir_temp$`NIV-Cost_dis`[j]<-cir_temp$`NIV-Cost`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Cost_pfs`[j]<-0*sim_niv$PFS[j]
            cir_temp$`NIV-Cost_pfs_dis`[j]<-cir_temp$`NIV-Cost_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Utility`[j]<-u_base*sim_niv$PFS[j]+u_pd*sim_niv$PD[j]
            cir_temp$`NIV-Utility_dis`[j]<-cir_temp$`NIV-Utility`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`NIV-Utility_pfs`[j]<-u_base*sim_niv$PFS[j]
            cir_temp$`NIV-Utility_pfs_dis`[j]<-cir_temp$`NIV-Utility_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            
            cir_temp$`IPI-Cost`[j]<-0*sim_ipi$PFS[j]+pd_cost_ipi*sim_ipi$PD[j]+c_eol*(sim_niv$Death[j]-sim_niv$Death[j-1])
            cir_temp$`IPI-Cost_dis`[j]<-cir_temp$`IPI-Cost`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Cost_pfs`[j]<-0*sim_ipi$PFS[j]
            cir_temp$`IPI-Cost_pfs_dis`[j]<-cir_temp$`IPI-Cost_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Utility`[j]<-u_base*sim_ipi$PFS[j]+u_pd*sim_ipi$PD[j]
            cir_temp$`IPI-Utility_dis`[j]<-cir_temp$`IPI-Utility`[j]/(1+discount)^(cir_temp$Time[j]-1)
            cir_temp$`IPI-Utility_pfs`[j]<-u_base*sim_ipi$PFS[j]
            cir_temp$`IPI-Utility_pfs_dis`[j]<-cir_temp$`IPI-Utility_pfs`[j]/(1+discount)^(cir_temp$Time[j]-1)
          }
          #summary results
          sum_res_temp<-as.data.frame(array(dim = c(1,18)))  
          colnames(sum_res_temp)<-colnames(cir_temp)
          sum_res_temp<-sum_res_temp[1,3:18]
          sum_res_temp$`NIV-Cost`<-sum(cir_temp$`NIV-Cost`)+c_ae12_pd_nivipi+c_ae12_pfs_nivipi+c_ae34_pfs_nivipi+c_ae34_pd_nivipi
          sum_res_temp$`NIV-Cost_dis`<-sum(cir_temp$`NIV-Cost_dis`)+c_ae12_pd_nivipi+c_ae12_pfs_nivipi+c_ae34_pfs_nivipi+c_ae34_pd_nivipi
          sum_res_temp$`NIV-Cost_pfs`<-sum(cir_temp$`NIV-Cost_pfs`)+c_ae12_pfs_nivipi+c_ae34_pfs_nivipi
          sum_res_temp$`NIV-Cost_pfs_dis`<-sum(cir_temp$`NIV-Cost_pfs_dis`)+c_ae12_pfs_nivipi+c_ae34_pfs_nivipi
          sum_res_temp$`NIV-Utility`<-sum(cir_temp$`NIV-Utility`)/12+(disu_ae12_pd_nivipi+disu_ae12_pfs_nivipi+disu_ae34_pfs_nivipi+disu_ae34_pd_nivipi)/12
          sum_res_temp$`NIV-Utility_dis`<-sum(cir_temp$`NIV-Utility_dis`)/12+(disu_ae12_pd_nivipi+disu_ae12_pfs_nivipi+disu_ae34_pfs_nivipi+disu_ae34_pd_nivipi)/12
          sum_res_temp$`NIV-Utility_pfs`<-sum(cir_temp$`NIV-Utility_pfs`)/12+(disu_ae12_pfs_nivipi+disu_ae34_pfs_nivipi)/12
          sum_res_temp$`NIV-Utility_pfs_dis`<-sum(cir_temp$`NIV-Utility_pfs_dis`)/12+(disu_ae12_pfs_nivipi+disu_ae34_pfs_nivipi)/12
          
          sum_res_temp$`IPI-Cost`<-sum(cir_temp$`IPI-Cost`)+c_ae12_pd_ipi+c_ae12_pfs_ipi+c_ae34_pfs_ipi+c_ae34_pd_ipi
          sum_res_temp$`IPI-Cost_dis`<-sum(cir_temp$`IPI-Cost_dis`)+c_ae12_pd_ipi+c_ae12_pfs_ipi+c_ae34_pfs_ipi+c_ae34_pd_ipi
          sum_res_temp$`IPI-Cost_pfs`<-sum(cir_temp$`IPI-Cost_pfs`)+c_ae12_pfs_ipi+c_ae34_pfs_ipi
          sum_res_temp$`IPI-Cost_pfs_dis`<-sum(cir_temp$`IPI-Cost_pfs_dis`)+c_ae12_pfs_ipi+c_ae34_pfs_ipi
          sum_res_temp$`IPI-Utility`<-sum(cir_temp$`IPI-Utility`)/12+(disu_ae12_pd_ipi+disu_ae12_pfs_ipi+disu_ae34_pfs_ipi+disu_ae34_pd_ipi)/12
          sum_res_temp$`IPI-Utility_dis`<-sum(cir_temp$`IPI-Utility_dis`)/12+(disu_ae12_pd_ipi+disu_ae12_pfs_ipi+disu_ae34_pfs_ipi+disu_ae34_pd_ipi)/12
          sum_res_temp$`IPI-Utility_pfs`<-sum(cir_temp$`IPI-Utility_pfs`)/12+(disu_ae12_pfs_ipi+disu_ae34_pfs_ipi)/12
          sum_res_temp$`IPI-Utility_pfs_dis`<-sum(cir_temp$`IPI-Utility_pfs_dis`)/12+(disu_ae12_pfs_ipi+disu_ae34_pfs_ipi)/12
          
          ICER<-(sum_res_temp$`NIV-Cost_dis` - sum_res_temp$`IPI-Cost_dis`)/(sum_res_temp$`NIV-Utility_dis` - sum_res_temp$`IPI-Utility_dis`)
          ICER_pfs<-(sum_res_temp$`NIV-Cost_pfs_dis` - sum_res_temp$`IPI-Cost_pfs_dis`)/(sum_res_temp$`NIV-Utility_pfs_dis` - sum_res_temp$`IPI-Utility_pfs_dis`)
          
          ICER_res$Combination[index]<-paste(a,b,c,d)
          ICER_res$ICER[index]<-ICER
          ICER_res$ICER_PFS[index]<-ICER_pfs
          ICER_res$INB[index]<-(sum_res_temp$`NIV-Utility_dis` - sum_res_temp$`IPI-Utility_dis`)*WTP-(sum_res_temp$`NIV-Cost_dis` - sum_res_temp$`IPI-Cost_dis`)
          ICER_res$Cost[index]<-(sum_res_temp$`NIV-Cost_dis` - sum_res_temp$`IPI-Cost_dis`)
          ICER_res$QALY[index]<-(sum_res_temp$`NIV-Utility_dis` - sum_res_temp$`IPI-Utility_dis`)
          index=index+1
        }
      }
    }
  }
}
#####################

ICER_res_new <- ICER_res %>% filter(Combination != "error")


len<-length(ICER_res_new$Combination)
cod<-ICER_res_new$Combination
cod_new<-as.data.frame(array(dim=c(len,4)))
for (i in 1:len) {
  for (j in 1:4) {
    cod_new[i,j]<-strsplit(cod[i]," ")[[1]][j]
  }
}

for (i in 1:len) {
  for (j in 1:4) {
    if(j==1){
      for (k in 1:3) {
        if(cod_new[i,j]=="1") {cod_new[i,j]=best_model[j,1]}
        else if(cod_new[i,j]=="2") {cod_new[i,j]=best_model[j,2]}
        else if(cod_new[i,j]=="3") {cod_new[i,j]=best_model[j,3]}
      }
    }
    if(j==2){
      for (k in 1:3) {
        if(cod_new[i,j]=="1") {cod_new[i,j]=best_model[j,1]}
        else if(cod_new[i,j]=="2") {cod_new[i,j]=best_model[j,2]}
        else if(cod_new[i,j]=="3") {cod_new[i,j]=best_model[j,3]}
      }
    }
    if(j==3){
      for (k in 1:3) {
        if(cod_new[i,j]=="1") {cod_new[i,j]=best_model[j,1]}
        else if(cod_new[i,j]=="2") {cod_new[i,j]=best_model[j,2]}
        else if(cod_new[i,j]=="3") {cod_new[i,j]=best_model[j,3]}
      }
    }
    if(j==4){
      for (k in 1:3) {
        if(cod_new[i,j]=="1") {cod_new[i,j]=best_model[j,1]}
        else if(cod_new[i,j]=="2") {cod_new[i,j]=best_model[j,2]}
        else if(cod_new[i,j]=="3") {cod_new[i,j]=best_model[j,3]}
      }
    }
  }
}
cod_final<-array(dim=c(len,1))
for (i in 1:len) {
  cod_final[i]<-paste(cod_new[i,],collapse=" - ")
}

ICER_res_new$Combination<-cod_final
colnames(ICER_res_new)<-c("nivos-nivpfs-ipios-ipipfs","ICER","ICER_PFS","INB","Incremental_Cost","Incremental_QALY")
summary(ICER_res_new)

# write.csv(ICER_res_new,"ICER_6.5fit_6.5.csv",row.names = FALSE)
# write.csv(ICER_res_new,"ICER_6.5fit_20.csv",row.names = FALSE)
# write.csv(ICER_res_new,"ICER_3fit_6.5.csv",row.names = FALSE)
# write.csv(ICER_res_new,"ICER_3fit_20.csv",row.names = FALSE)
# write.csv(ICER_res_new,"ICER_3ex_6.5.csv",row.names = FALSE)
# write.csv(ICER_res_new,"ICER_3ex_20.csv",row.names = FALSE)


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

# 6.5 fit
# icer_6.5_6.5ys<-
#   ggplot(dfdelta, aes(x = Ie, y =Ic)) +
#                 geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
#                 geom_point(aes(y=Ic[58] ,x=Ie[58]),shape=16,colour="red",size=4)+
#                 scale_fill_continuous()+
#                 # stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
#                 coord_cartesian(ylim=c(-3,3),xlim = c(-3,3))+
#                 theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
#                          axis.title = element_text(size = 16),plot.title = element_text(size = 16),plot.subtitle = element_text(size = 14))+
#                 labs(title="ICER 6.5-year horizon",
#                      subtitle = "6.5Ys-fit data",
#                      x="Incremental QALYs(standardized)",
#                      y="Incremental Cost(standardized)")+
#                 # geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
#                 stat_function(fun=aes_plot, geom="line",xlim = c(-3,3),size=1,colour="black",linetype="dashed")
# icer_6.5_6.5ys

# icer_20_6.5ys<-
#   ggplot(dfdelta, aes(x = Ie, y =Ic)) +
#   geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
#   geom_point(aes(y=Ic[69] ,x=Ie[69]),shape=16,colour="red",size=4)+
#   scale_fill_continuous()+
#   # stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
#   coord_cartesian(ylim=c(-3,3),xlim = c(-3,3))+
#   theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
#                                   axis.title = element_text(size = 16),plot.title = element_text(size = 16),plot.subtitle = element_text(size = 14))+
#   labs(title="ICER 20-year horizon",
#        subtitle = "6.5Ys-fit data",
#        x="Incremental QALYs(standardized)",
#        y="Incremental Cost(standardized)")+
#   # geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
#   stat_function(fun=aes_plot, geom="line",xlim = c(-3,3),size=1,colour="black",linetype="dashed")
# icer_20_6.5ys

# # 3 fit
# icer_6.5_3ysfit<-
#   ggplot(dfdelta, aes(x = Ie, y =Ic)) +
#                 geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
#                 geom_point(aes(y=Ic[30] ,x=Ie[30]),shape=16,colour="red",size=4)+
#                 scale_fill_continuous()+
#                 # stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
#                 coord_cartesian(ylim=c(-3,3),xlim = c(-3,3))+
#   theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
#         axis.title = element_text(size = 16),plot.title = element_text(size = 16),plot.subtitle = element_text(size = 14))+
#                 labs(title="ICER 6.5-year horizon",
#                      subtitle = "3Ys-fit data",
#                      x="Incremental QALYs(standardized)",
#                      y="Incremental Cost(standardized)")+
#                 # geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
#                 stat_function(fun=aes_plot, geom="line",xlim = c(-3,3),size=1,colour="black",linetype="dashed")
# icer_6.5_3ysfit

# icer_20_3ysfit<-
#   ggplot(dfdelta, aes(x = Ie, y =Ic)) +
#                 geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
#                 geom_point(aes(y=Ic[39] ,x=Ie[39]),shape=16,colour="red",size=4)+
#                 scale_fill_continuous()+
#                 # stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
#                 coord_cartesian(ylim=c(-3,3),xlim = c(-3,3))+
#   theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
#         axis.title = element_text(size = 16),plot.title = element_text(size = 16),plot.subtitle = element_text(size = 14))+
#                 labs(title="ICER 20-year horizon",
#                      subtitle = "3Ys-fit data",
#                      x="Incremental QALYs(standardized)",
#                      y="Incremental Cost(standardized)")+
#                 # geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
#                 stat_function(fun=aes_plot, geom="line",xlim = c(-3,3),size=1,colour="black",linetype="dashed")
# icer_20_3ysfit

# # 3 extrapolate
# icer_6.5_3ysextra<-
#   ggplot(dfdelta, aes(x = Ie, y =Ic)) +
#                 geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
#                 geom_point(aes(y=Ic[22] ,x=Ie[22]),shape=16,colour="red",size=4)+
#                 scale_fill_continuous()+
#                 # stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
#                 coord_cartesian(ylim=c(-3,3),xlim = c(-3,3))+
#   theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
#         axis.title = element_text(size = 16),plot.title = element_text(size = 16),plot.subtitle = element_text(size = 14))+
#                 labs(title="ICER 6.5-year horizon",
#                      subtitle = "3Ys-extrapolate data",
#                      x="Incremental QALYs(standardized)",
#                      y="Incremental Cost(standardized)")+
#                 # geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
#                 stat_function(fun=aes_plot, geom="line",xlim = c(-3,3),size=1,colour="black",linetype="dashed")
# icer_6.5_3ysextra

# icer_20_3ysextra<-
#   ggplot(dfdelta, aes(x = Ie, y =Ic)) +
#                 geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
#                 geom_point(aes(y=Ic[4] ,x=Ie[4]),shape=16,colour="red",size=4)+
#                 scale_fill_continuous()+
#                 # stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
#                 coord_cartesian(ylim=c(-3,3),xlim = c(-3,3))+
#   theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16),
#         axis.title = element_text(size = 16),plot.title = element_text(size = 16),plot.subtitle = element_text(size = 14))+
#                 labs(title="ICER 20-year horizon",
#                      subtitle = "3Ys-extrapolate data",
#                      x="Incremental QALYs(standardized)",
#                      y="Incremental Cost(standardized)")+
#                 # geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
#                 stat_function(fun=aes_plot, geom="line",xlim = c(-3,3),size=1,colour="black",linetype="dashed")
# icer_20_3ysextra


icer_plot_combined<-grid.arrange(icer_6.5_6.5ys,icer_20_6.5ys,
                                 icer_6.5_3ysfit,icer_20_3ysfit,
                                 icer_6.5_3ysextra,icer_20_3ysextra,
                                 ncol=2, widths=c(18, 18))

ggsave("icer_plot_combined_scaled.png",plot=icer_plot_combined,width = 16,height = 12,dpi = 1200)





#######################
#original plot
#######################

# reference0<-data.frame(Cost=dfdelta$Ic[1],QALY=dfdelta$Ie[1])
# delta0<-as.data.frame(matrix(ncol = 1,nrow = (length(dfdelta$Ic)-1)))
# 
# for (i in 2:length(dfdelta$Ic)) {
#   delta0[i-1,1] = sqrt((dfdelta[i,1]-reference0$Cost)^2+(dfdelta[i,2]-reference0$QALY)^2)
# }
# mean(delta0$V1,na.rm = TRUE)
# sd(delta0$V1,na.rm = TRUE)

# wtp_line<-function(x){WTP*x}
# aes_plot<-function(x){0}

# ## 6.5 fit
# icer_6.5_6.5ys<-
#   ggplot(ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost)) +
#                 geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
#                 geom_point(aes(y=Incremental_Cost[1] ,x=Incremental_QALY[1]),shape=16,colour="red",size=4)+
#                 scale_fill_continuous()+
#                 stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
#                 coord_cartesian(ylim=c(0,120000),xlim = c(0,2))+
#                 theme_bw()+
#                 labs(title="ICER 6.5-year horizon",
#                      subtitle = "6.5Ys-fit data")+
#                 geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
#                 stat_function(fun=aes_plot, geom="line",xlim = c(-0.1,3),size=1,colour="black",linetype="dashed")
# icer_6.5_6.5ys
# 
# icer_20_6.5ys<-
# ggplot(ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost)) +
# geom_point(size=3,shape=16,colour="royalblue")+
# geom_point(aes(y=Incremental_Cost[1] ,x=Incremental_QALY[1]),shape=16,colour="red",size=4)+
# scale_fill_continuous()+
# stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,6),size=1,colour="hotpink")+
# coord_cartesian(ylim=c(-80000,10000),xlim = c(0,5))+
# theme_bw()+
# labs(title="ICER 20-year horizon",
#      subtitle = "6.5Ys-fit data")+
# geom_text(aes(x= 1.2, y = -10000, label = "WTP = $64,000"),size=4)+
# stat_function(fun=aes_plot, geom="line",xlim = c(-0.5,6),size=1,colour="black",linetype="dashed")
# icer_20_6.5ys

## 3 fit
# icer_6.5_3ysfit<-
#   ggplot(ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost)) +
#                 geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
#                 geom_point(aes(y=Incremental_Cost[1] ,x=Incremental_QALY[1]),shape=16,colour="red",size=4)+
#                 scale_fill_continuous()+
#                 stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
#                 coord_cartesian(ylim=c(0,120000),xlim = c(0,2))+
#                 theme_bw()+
#                 labs(title="ICER 6.5-year horizon",
#                      subtitle = "3Ys-fit data")+
#                 geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
#                 stat_function(fun=aes_plot, geom="line",xlim = c(-0.1,3),size=1,colour="black",linetype="dashed")
# icer_6.5_3ysfit

# icer_20_3ysfit<-
# ggplot(ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost)) +
# geom_point(size=3,shape=16,colour="royalblue")+
# geom_point(aes(y=Incremental_Cost[1] ,x=Incremental_QALY[1]),shape=16,colour="red",size=4)+
# scale_fill_continuous()+
# stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,6),size=1,colour="hotpink")+
# coord_cartesian(ylim=c(0,150000),xlim = c(0,5))+
# theme_bw()+
# labs(title="ICER 20-year horizon",
#      subtitle = "3Ys-fit data")+
# geom_text(aes(x= 1.2, y = 10000, label = "WTP = $64,000"),size=4)+
# stat_function(fun=aes_plot, geom="line",xlim = c(-0.5,6),size=1,colour="black",linetype="dashed")
# icer_20_3ysfit

# 3 fit
# icer_6.5_3ysextra<-
#   ggplot(ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost)) +
#                 geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
#                 geom_point(aes(y=Incremental_Cost[1] ,x=Incremental_QALY[1]),shape=16,colour="red",size=4)+
#                 scale_fill_continuous()+
#                 stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
#                 coord_cartesian(ylim=c(0,120000),xlim = c(0,2))+
#                 theme_bw()+
#                 labs(title="ICER 6.5-year horizon",
#                      subtitle = "3Ys-extrapolate data")+
#                 geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
#                 stat_function(fun=aes_plot, geom="line",xlim = c(-0.1,3),size=1,colour="black",linetype="dashed")
# icer_6.5_3ysextra

# icer_20_3ysextra<-
# ggplot(ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost)) +
# geom_point(size=3,shape=16,colour="royalblue")+
# geom_point(aes(y=Incremental_Cost[1] ,x=Incremental_QALY[1]),shape=16,colour="red",size=4)+
# scale_fill_continuous()+
# stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,6),size=1,colour="hotpink")+
# coord_cartesian(ylim=c(-300000,50000),xlim = c(0,5))+
# theme_bw()+
# labs(title="ICER 20-year horizon",
#      subtitle = "3Ys-extrapolate data")+
# geom_text(aes(x= 1.2, y = 20000, label = "WTP = $64,000"),size=4)+
# stat_function(fun=aes_plot, geom="line",xlim = c(-0.5,6),size=1,colour="black",linetype="dashed")
# icer_20_3ysextra


# icer_plot_combined<-grid.arrange(icer_6.5_6.5ys,icer_20_6.5ys,
#                                  icer_6.5_3ysfit,icer_20_3ysfit,
#                                  icer_6.5_3ysextra,icer_20_3ysextra,
#                                  ncol=2, widths=c(18, 18))
# 
# ggsave("icer_plot_combined.png",plot=icer_plot_combined,width = 16,height = 12,dpi = 1200)



























# # #3 fit
# icer_6.5_3ysfit<-
#   ggplot(ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost)) +
#   geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
#   geom_point(aes(y=Incremental_Cost[224] ,x=Incremental_QALY[224]),shape=16,colour="red",size=4)+
#   scale_fill_continuous()+
#   stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,5),size=1,colour="hotpink")+
#   coord_cartesian(ylim=c(0,100000),xlim = c(0.5,1.5))+
#   theme_bw()+
#   labs(title="ICER 6.5-year horizon",
#        subtitle = "3Ys-fit data")+
#   geom_text(aes(x= 0.75, y = 36000, label = "WTP = $64,000"),size=4)+
#   stat_function(fun=aes_plot, geom="line",xlim = c(-0.1,5),size=1,colour="black",linetype="dashed")
# icer_6.5_3ysfit

# icer_20_3ysfit<-
#   ggplot(ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost)) +
#   geom_point(size=3,shape=16,colour="royalblue")+
#   geom_point(aes(y=Incremental_Cost[168] ,x=Incremental_QALY[168]),shape=16,colour="red",size=4)+
#   scale_fill_continuous()+
#   stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,7),size=1,colour="hotpink")+
#   coord_cartesian(ylim=c(-300000,200000),xlim = c(2,5))+
#   theme_bw()+
#   labs(title="ICER 20-year horizon",
#        subtitle = "3Ys-fit data")+
#   geom_text(aes(x= 2.5, y = 100000, label = "WTP = $64,000"),size=4)+
#   stat_function(fun=aes_plot, geom="line",xlim = c(-0.5,7),size=1,colour="black",linetype="dashed")
# ## 3 extra
# icer_6.5_3ysex<-
#   ggplot(ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost)) +
#   geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
#   geom_point(aes(y=Incremental_Cost[151] ,x=Incremental_QALY[151]),shape=16,colour="red",size=4)+
#   scale_fill_continuous()+
#   stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,3),size=1,colour="hotpink")+
#   coord_cartesian(ylim=c(0,120000),xlim = c(0,2))+
#   theme_bw()+
#   labs(title="ICER 6.5-year horizon",
#        subtitle = "3Ys-extrapolate data")+
#   geom_text(aes(x= 0.6, y = 10000, label = "WTP = $64,000"),size=4)+
#   stat_function(fun=aes_plot, geom="line",xlim = c(-0.1,3),size=1,colour="black",linetype="dashed")
# #
# icer_20_3ysex<-
#   ggplot(ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost)) +
#   geom_point(size=3,shape=16,colour="royalblue")+
#   geom_point(aes(y=Incremental_Cost[84] ,x=Incremental_QALY[84]),shape=16,colour="red",size=4)+
#   scale_fill_continuous()+
#   stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,7),size=1,colour="hotpink")+
#   coord_cartesian(ylim=c(-210000,140000),xlim = c(0,5))+
#   theme_bw()+
#   labs(title="ICER 20-year horizon",
#        subtitle = "3Ys-extrapolate data")+
#   geom_text(aes(x= 1.2, y = 25000, label = "WTP = $64,000"),size=4)+
#   stat_function(fun=aes_plot, geom="line",xlim = c(-0.5,7),size=1,colour="black",linetype="dashed")

# icer_plot_combined<-grid.arrange(icer_6.5_6.5ys,icer_20_6.5ys,
#                                  icer_6.5_3ysfit,icer_20_3ysfit,
#                                  icer_6.5_3ysex,icer_20_3ysex,
#                                  ncol=2, widths=c(18, 18))
# 
# ggsave("icer_plot_combined.png",plot=icer_plot_combined,width = 16,height = 12,dpi = 1200)


# setwd("C:/Users/ECHO/Desktop/PE-flexible/CEA-MODEL/LE-data/data")
# write.csv(icer_6.5_6.5ys$data,"icer_6.5_6.5ys.csv")
# write.csv(icer_20_6.5ys$data,"icer_20_6.5ys.csv")
# write.csv(icer_6.5_3ysfit$data,"icer_6.5_3ysfit.csv")
# write.csv(icer_20_3ysfit$data,"icer_20_3ysfit.csv")
# write.csv(icer_6.5_3ysex$data,"icer_6.5_3ysex.csv")
# write.csv(icer_20_3ysex$data,"icer_20_3ysex.csv")



# # 6.5ys data
# # 20ys
# RES_6.5y_20<-as.data.frame(array(dim=c(5,6)))
# colnames(RES_6.5y_20)<-c("model","ICER","ICER_PFS","INB","Incremental_Cost","Incremental_QALY")
# RES_6.5y_20[1,]<-ICER_res_new[218,]
# RES_6.5y_20[2,]<-ICER_res_new[1,]
# RES_6.5y_20[3,]<-ICER_res_new[86,]
# RES_6.5y_20[4,]<-ICER_res_new[171,]
# RES_6.5y_20[5,]<-ICER_res_new[256,]
# RES_6.5y_20[,1]<-c("best","Standard survival model","FP","RCS","RP")
# 
# # 6.5ys
# RES_6.5y_6.5<-as.data.frame(array(dim=c(5,6)))
# colnames(RES_6.5y_6.5)<-c("model","ICER","ICER_PFS","INB","Incremental_Cost","Incremental_QALY")
# RES_6.5y_6.5[1,]<-ICER_res_new[218,]
# RES_6.5y_6.5[2,]<-ICER_res_new[1,]
# RES_6.5y_6.5[3,]<-ICER_res_new[86,]
# RES_6.5y_6.5[4,]<-ICER_res_new[171,]
# RES_6.5y_6.5[5,]<-ICER_res_new[256,]
# RES_6.5y_6.5[,1]<-c("best","Standard survival model","FP","RCS","RP")
# 
# table_20<-t(RES_6.5y_20)
# table_6.5<-t(RES_6.5y_6.5)
# write.csv(table_20,"table_20.csv")
# write.csv(table_6.5,"table_6.5.csv")

icer_plot_combined<-grid.arrange(icer_6.5_6.5ys,icer_20_6.5ys,
                                 icer_6.5_3ys,icer_20_3ys,
                                 ncol=2, widths=c(18, 18))
ggsave("icer_combined_new.png",plot=icer_plot_combined,width = 16,height = 12,dpi = 600)