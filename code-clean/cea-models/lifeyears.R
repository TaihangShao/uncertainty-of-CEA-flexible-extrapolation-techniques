# library("hesim")
library("flexsurv")
library("ggplot2")
library("data.table")
library("tidyverse")
library("tidyr")
library("tidygraph")
library("gridExtra")


##############################
# load data 6.5ys 
##############################
setwd("C:/Users/Administrator/Desktop/pe-flex-revise/PE-submission-final/new-codes/cea-models/6.5ys-fit")

niv_pfs<-read.csv("6.5-niv_pfs_dfsurv_20y.csv")
niv_os<-read.csv("6.5-niv_os_dfsurv_20y.csv")
ipi_pfs<-read.csv("6.5-ipi_pfs_dfsurv_20y.csv")
ipi_os<-read.csv("6.5-ipi_os_dfsurv_20y.csv")

LYs<-as.data.frame(matrix(nrow = 16,ncol = 8))

for (i in 1: 16) {
  LYs[i,1]<-sum(niv_pfs[,i+2])*21/365.25
  LYs[i,2]<-sum(niv_os[,i+2])*21/365.25
  LYs[i,3]<-sum(ipi_pfs[,i+2])*21/365.25
  LYs[i,4]<-sum(ipi_os[,i+2])*21/365.25
}




##############################
## load data 3ys 
##############################
setwd("C:/Users/Administrator/Desktop/pe-flex-revise/PE-submission-final/new-codes/cea-models/3ys")

niv_pfs<-read.csv("niv_pfs_dfsurv_20y.csv")
niv_os<-read.csv("niv_os_dfsurv_20y.csv")
ipi_pfs<-read.csv("ipi_pfs_dfsurv_20y.csv")
ipi_os<-read.csv("ipi_os_dfsurv_20y.csv")

for (i in 1: 16) {
  LYs[i,5]<-sum(niv_pfs[,i+2])*21/365.25
  LYs[i,6]<-sum(niv_os[,i+2])*21/365.25
  LYs[i,7]<-sum(ipi_pfs[,i+2])*21/365.25
  LYs[i,8]<-sum(ipi_os[,i+2])*21/365.25
}

setwd("C:/Users/Administrator/Desktop/pe-flex-revise/PE-submission-final/new-codes/cea-models")
write.csv(LYs,"lifeyears.csv",row.names = FALSE)





