## load data 3ys best fit
#
niv_pfs<-read.csv("niv-pfs-3y-LE.csv")
niv_os<-read.csv("niv-os-3y-LE.csv")
ipi_pfs<-read.csv("ipi-pfs-3y-LE.csv")
ipi_os<-read.csv("ipi-os-3y-LE.csv")

niv_os<-data.frame(niv_os$Time,niv_os$gompertz,niv_os$FP1.2,niv_os$RCS1,niv_os$RP.odds.1)
niv_pfs<-data.frame(niv_pfs$Time,niv_pfs$genf,niv_pfs$FP2.1,niv_pfs$RCS2,niv_pfs$RP.hazard.2 )
ipi_os<-data.frame(ipi_os$Time,ipi_os$lnorm,ipi_os$FP2.1,ipi_os$RCS1,ipi_os$RP.normal.1)
ipi_pfs<-data.frame(ipi_pfs$Time,ipi_pfs$gengamma,ipi_pfs$FP2.1,ipi_pfs$RCS2,ipi_pfs$RP.hazard.1)

## load data 3ys best extra
#
niv_pfs<-read.csv("niv-pfs-3y-LE.csv")
niv_os<-read.csv("niv-os-3y-LE.csv")
ipi_pfs<-read.csv("ipi-pfs-3y-LE.csv")
ipi_os<-read.csv("ipi-os-3y-LE.csv")

niv_os<-data.frame(niv_os$Time,niv_os$genf,niv_os$FP1.1,niv_os$RCS2,niv_os$RP.normal.2)
niv_pfs<-data.frame(niv_pfs$Time,niv_pfs$gompertz,niv_pfs$FP2.1,niv_pfs$RCS1,niv_pfs$RP.normal.2 )
ipi_os<-data.frame(ipi_os$Time,ipi_os$genf,ipi_os$FP2.2,ipi_os$RCS1,ipi_os$RP.odds.1)
ipi_pfs<-data.frame(ipi_pfs$Time,ipi_pfs$gompertz,ipi_pfs$FP2.1,ipi_pfs$RCS1,ipi_pfs$RP.hazard.1)

##########################
###  rename
##########################
colnames(niv_os)<-c("Time","Standard Dist","FP","RCS","RP")
colnames(niv_pfs)<-c("Time","Standard Dist","FP","RCS","RP")
colnames(ipi_os)<-c("Time","Standard Dist","FP","RCS","RP")
colnames(ipi_pfs)<-c("Time","Standard Dist","FP","RCS","RP")

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
ICER_res<-as.data.frame(array(dim=c(4^4,6)))
colnames(ICER_res)<-c("Combination","ICER","ICER_PFS","INB","Cost","QALY")
a<-1
b<-1
c<-1
d<-1
index<-1

## for cycle 4^4 ICER
# PSM
for (a in 1:4) {
  
  # half cycle correction
  pfs_niv<-niv_pfs[,a+1]
  pfs_niv<-c(1,pfs_niv)
  for (i in 1:cycle) {
    pfs_niv[i]<-(pfs_niv[i]+pfs_niv[i+1])/2
  }
  
  for (b in 1:4) {
    # half cycle correction
    os_niv<-niv_os[,b+1]
    os_niv<-c(1,os_niv)
    for (i in 1:cycle) {
      os_niv[i]<-(os_niv[i]+os_niv[i+1])/2
    }
    
    for (c in 1:4) {
      # half cycle correction
      pfs_ipi<-ipi_pfs[,c+1]
      pfs_ipi<-c(1,pfs_ipi)
      for (i in 1:cycle) {
        pfs_ipi[i]<-(pfs_ipi[i]+pfs_ipi[i+1])/2
      }
      
      for (d in 1:4) {
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
    if(cod_new[i,j]=="1") {cod_new[i,j]="standard dist"}
    else if(cod_new[i,j]=="2") {cod_new[i,j]="FP"}
    else if(cod_new[i,j]=="3") {cod_new[i,j]="RCS"}
    else if(cod_new[i,j]=="4") {cod_new[i,j]="RP"}
  }
}
cod_final<-array(dim=c(len,1))
for (i in 1:len) {
  cod_final[i]<-paste(cod_new[i,],collapse=" - ")
}

ICER_res_new$Combination<-cod_final
colnames(ICER_res_new)<-c("nivpfs-nivos-ipipfs-ipios","ICER","ICER_PFS","INB","Incremental_Cost","Incremental_QALY")
summary(ICER_res_new)

wtp_line<-function(x){WTP*x}
aes_plot<-function(x){0}




# #3 fit
icer_6.5_3ysfit<-
  ggplot(ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost)) +
  geom_point(size=3,shape=16,colour="royalblue",fill="bisque1")+
  geom_point(aes(y=Incremental_Cost[224] ,x=Incremental_QALY[224]),shape=16,colour="red",size=4)+
  scale_fill_continuous()+
  stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,5),size=1,colour="hotpink")+
  coord_cartesian(ylim=c(0,100000),xlim = c(0.5,1.5))+
  theme_bw()+
  labs(title="ICER 6.5-year horizon",
       subtitle = "3Ys-fit data")+
  geom_text(aes(x= 0.75, y = 36000, label = "WTP = $64,000"),size=4)+
  stat_function(fun=aes_plot, geom="line",xlim = c(-0.1,5),size=1,colour="black",linetype="dashed")
icer_6.5_3ysfit

##################################################################--###
ICER_res_new0 <- ICER_res %>% filter(Combination != "error")

len<-length(ICER_res_new0$Combination)
cod<-ICER_res_new0$Combination
cod_new<-as.data.frame(array(dim=c(len,4)))
for (i in 1:len) {
  for (j in 1:4) {
    cod_new[i,j]<-strsplit(cod[i]," ")[[1]][j]
  }
}

for (i in 1:len) {
  for (j in 1:4) {
    if(cod_new[i,j]=="1") {cod_new[i,j]="standard dist"}
    else if(cod_new[i,j]=="2") {cod_new[i,j]="FP"}
    else if(cod_new[i,j]=="3") {cod_new[i,j]="RCS"}
    else if(cod_new[i,j]=="4") {cod_new[i,j]="RP"}
  }
}
cod_final<-array(dim=c(len,1))
for (i in 1:len) {
  cod_final[i]<-paste(cod_new[i,],collapse=" - ")
}

ICER_res_new0$Combination<-cod_final
colnames(ICER_res_new0)<-c("nivpfs-nivos-ipipfs-ipios","ICER","ICER_PFS","INB","Incremental_Cost","Incremental_QALY")
summary(ICER_res_new0)



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


icer_20_3ysfit<-
  ggplot(ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost)) +
  geom_point(size=3,shape=16,colour="royalblue")+
  geom_point(aes(y=Incremental_Cost[168] ,x=Incremental_QALY[168]),shape=16,colour="red",size=4)+
  scale_fill_continuous()+
  stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,7),size=1,colour="hotpink")+
  coord_cartesian(ylim=c(-300000,200000),xlim = c(2,5))+
  theme_bw()+
  labs(title="ICER 20-year horizon",
       subtitle = "3Ys-fit data")+
  geom_text(aes(x= 2.5, y = 100000, label = "WTP = $64,000"),size=4)+
  stat_function(fun=aes_plot, geom="line",xlim = c(-0.5,7),size=1,colour="black",linetype="dashed")

icer_20_3ysex<-
  ggplot(ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost)) +
  geom_point(size=3,shape=16,colour="royalblue")+
  geom_point(aes(y=Incremental_Cost[84] ,x=Incremental_QALY[84]),shape=16,colour="red",size=4)+
  scale_fill_continuous()+
  stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,7),size=1,colour="hotpink")+
  coord_cartesian(ylim=c(-210000,140000),xlim = c(0,5))+
  theme_bw()+
  labs(title="ICER 20-year horizon",
       subtitle = "3Ys-extrapolate data")+
  geom_text(aes(x= 1.2, y = 25000, label = "WTP = $64,000"),size=4)+
  stat_function(fun=aes_plot, geom="line",xlim = c(-0.5,7),size=1,colour="black",linetype="dashed")



icer_20_3ys<-
  ggplot()+geom_point(data=ICER_res_new, aes(x = Incremental_QALY, y =Incremental_Cost),size=3,shape=16,colour="seagreen4")+
  geom_point(data=ICER_res_new, aes(y=Incremental_Cost[168] ,x=Incremental_QALY[168]),shape=16,colour="red",size=4)+
  geom_point(data=ICER_res_new0, aes(x = Incremental_QALY, y =Incremental_Cost),size=3,shape=16,colour="orange")+
  geom_point(data=ICER_res_new0, aes(y=Incremental_Cost[84] ,x=Incremental_QALY[84]),shape=17,colour="red",size=4)+
  scale_fill_continuous()+
  stat_function(fun=wtp_line, geom="line",xlim = c(-0.5,7),size=1,colour="hotpink")+
  coord_cartesian(ylim=c(-300000,200000),xlim = c(2,5))+
  theme_bw()+
  labs(title="ICER 20-year horizon",
       subtitle = "3Ys-fit and extrapolate data")+
  geom_text(aes(x= 2.5, y = 100000, label = "WTP = $64,000"),size=4)+
  stat_function(fun=aes_plot, geom="line",xlim = c(-0.5,7),size=1,colour="black",linetype="dashed")
icer_20_3ys
