library("tidyverse")   # Mainly for plotting
library("discSurv")    # Create life tables
library("flexsurv")    # RPs (also loads survival) and BC case-study
library("lme4")        # RE components
library("mgcv")        # GAM
library("KFAS")        # DSM
library("gridExtra")   # Plotting
library("zoo")         # Aid in time-series analyses
theme_set(theme_light())  # GGplot theme

md<-c("exp","weibull","gamma","lnorm","gompertz","llogis","gengamma","FP1","FP2","RCS","RP-hazard","RP-odds","RP-normal","GAM","param-mix","mix-cure")
# Get estimates of lifetime mean survival

fun_LE = function(haz, time){ # Function to estimate LE
  df = tibble(x = time, y = haz) %>%
    mutate(tau = x - lag(x, default=0),
           cum_y = cumsum(y * tau),
           surv =  exp(-cum_y),
           AUC = surv * tau)
  LE = sum(df$AUC)
  return(LE)
}


##niv-pfs

lthaz_3y<-read.csv(file = "lthaz-3y-nivpfs.csv")
lthaz_6.5y<-read.csv(file = "lthaz-6.5y-nivpfs.csv")
dfhaz<-read.csv(file = "dfhaz-3yex-nivpfs.csv")

ref_3y<-data.frame(time=lthaz_3y$Time,haz=lthaz_3y$hazKM)
haz_3y<-data.frame(time=lthaz_3y$Time,lthaz_3y[,12:27])


ref_6.5y<-data.frame(time=lthaz_6.5y$Time,haz=lthaz_6.5y$hazKM)
haz_6.5y<-data.frame(time=lthaz_6.5y$Time,lthaz_6.5y[,12:27])

dfhaz<-dfhaz[,-1]
dfhaz<-t(dfhaz)
colnames(dfhaz)<-md
dfhaz<-data.frame(time=lthaz_6.5y$Time,dfhaz)

p=6

LE_3y=round(fun_LE(ref_3y$haz,ref_3y$time),p)
LE_6.5y=round(fun_LE(ref_6.5y$haz,ref_6.5y$time),p)

res<-as.data.frame(matrix(nrow = 3,ncol = 16))
colnames(res)<-md


for (i in 1:16) {
  res[1,i]=round(fun_LE(haz_3y[,i+1],haz_3y$time),p)
}

for (i in 1:16) {
  res[2,i]=round(fun_LE(dfhaz[,i+1],dfhaz$time),p)
}

for (i in 1:16) {
  res[3,i]=round(fun_LE(haz_6.5y[,i+1],haz_6.5y$time),p)
}

delta_res<-res
delta_res[1,]=delta_res[1,]-LE_3y
delta_res[2,]=delta_res[2,]-LE_6.5y
delta_res[3,]=delta_res[3,]-LE_6.5y
delta_res<-delta_res*10000

write.csv(delta_res,"delta_res.csv")


