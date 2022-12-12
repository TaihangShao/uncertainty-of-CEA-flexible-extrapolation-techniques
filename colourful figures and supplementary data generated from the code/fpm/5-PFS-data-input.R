#For PFS
niv5y <- read.delim("IPD_pfs_niv_6.5y.txt")
niv5y<-data.frame(niv5y$event,niv5y$time)
niv5y<-rename(niv5y,"censrec"="niv5y.event","recyrs"="niv5y.time")
niv5y$recyrs<-niv5y$recyrs/12
niv5y$rectime<-as.integer(niv5y$recyrs*365.24) 

niv5y = niv5y %>% mutate(censrec = case_when(niv5y$recyrs > 6.5 ~ integer(1), TRUE ~ niv5y$censrec),
                         recyrs = case_when(niv5y$recyrs > 6.5 ~ 6.5, TRUE ~ niv5y$recyrs),
                         rectime = recyrs * 365)

niv3y <- read.delim("IPD_pfs_niv_3y.txt")
niv3y<-data.frame(niv3y$event,niv3y$time)
niv3y<-rename(niv3y,"censrec"="niv3y.event","recyrs"="niv3y.time")
niv3y$recyrs<-niv3y$recyrs/12
niv3y$rectime<-as.integer(niv3y$recyrs*365.24) 

niv3y = niv3y %>% mutate(censrec = case_when(niv3y$recyrs > 3 ~ integer(1), TRUE ~ niv3y$censrec),
                         recyrs = case_when(niv3y$recyrs > 3 ~ 3, TRUE ~ niv3y$recyrs),
                         rectime = recyrs * 365)

ipi5y <- read.delim("IPD_pfs_ipi_6.5y.txt")
ipi5y<-data.frame(ipi5y$event,ipi5y$time)
ipi5y<-rename(ipi5y,"censrec"="ipi5y.event","recyrs"="ipi5y.time")
ipi5y$recyrs<-ipi5y$recyrs/12
ipi5y$rectime<-as.integer(ipi5y$recyrs*365.24) 

ipi5y = ipi5y %>% mutate(censrec = case_when(ipi5y$recyrs > 6.5 ~ integer(1), TRUE ~ ipi5y$censrec),
                         recyrs = case_when(ipi5y$recyrs > 6.5 ~ 6.5, TRUE ~ ipi5y$recyrs),
                         rectime = recyrs * 365)

ipi3y <- read.delim("IPD_pfs_ipi_3y.txt")
ipi3y<-data.frame(ipi3y$event,ipi3y$time)
ipi3y<-rename(ipi3y,"censrec"="ipi3y.event","recyrs"="ipi3y.time")
ipi3y$recyrs<-ipi3y$recyrs/12
ipi3y$rectime<-as.integer(ipi3y$recyrs*365.24) 

ipi3y = ipi3y %>% mutate(censrec = case_when(ipi3y$recyrs > 3 ~ integer(1), TRUE ~ ipi3y$censrec),
                         recyrs = case_when(ipi3y$recyrs > 3 ~ 3, TRUE ~ ipi3y$recyrs),
                         rectime = recyrs * 365)

table(niv5y$censrec)
niv5y %>%
  summarise(Min_surv = min(recyrs, na.rm = TRUE),
            Max_surv = max(recyrs, na.rm = TRUE),
            Mean_surv = mean(recyrs, na.rm = TRUE),
            SD_surv = sd(recyrs, na.rm = TRUE))

table(niv3y$censrec)
niv3y %>%
  summarise(Min_surv = min(recyrs, na.rm = TRUE),
            Max_surv = max(recyrs, na.rm = TRUE),
            Mean_surv = mean(recyrs, na.rm = TRUE),
            SD_surv = sd(recyrs, na.rm = TRUE))

##-----niv data 3Y Monthly life table estimates of hazard-----###
niv3y$rectime2 <- as.integer(niv3y$rectime/(365.24/12)) + 1
niv5y$rectime2 <- as.integer(niv5y$rectime/(365.24/12)) + 1
#+1 above as integer rounds down, we want to round up
ltBC <- lifeTable(niv3y, timeColumn = "rectime2", eventColumn = "censrec")
ltHaz0 <- data.frame(hazKM = ltBC$Output$hazard, Time = (seq(1:length(ltBC$Output[,1]))-0.5)/12,
                     AtRisk = ltBC$Output$atRisk, Events = ltBC$Output$events)
# The above hazard is the product-limit (KM) estimate. Also calculate the life-table (acturial) estimate
ltHaz0$hazLT = ltHaz0$Events / (ltHaz0$AtRisk - ltHaz0$Events/2)
# Generate log-time
ltHaz0$lnTime <- log(ltHaz0$Time)
# For random effects add an ID for each time period
ltHaz0$MyId <- 1:dim(ltHaz0)[1] # Generate id variable 
# ltHaz0$re<-scale(ltHaz0$MyId)
# For AR(1) model get outcomes lagged by one.
ltHaz0$EventsL <- lag(ltHaz0$Events)
# Set first lagged value = 0 (usually would discard, but retain so IC are comparable. Can be justified as a prior value)
ltHaz0$EventsL[1] <- 0
#Set surv data
ltHaz0$surv <- ltBC$Output$S
#timedelta
ltHaz0$timedelta<-ltHaz0$Time[2]-ltHaz0$Time[1]

##-----niv data 5Y Monthly life table estimates of hazard-----### 
ltBC_out <- lifeTable(niv5y, timeColumn = "rectime2", eventColumn = "censrec")
ltHaz0_out <- data.frame(hazKM = ltBC_out$Output$hazard, Time = (seq(1:length(ltBC_out$Output[,1]))-0.5)/12,
                         AtRisk = ltBC_out$Output$atRisk, Events = ltBC_out$Output$events)
# The above hazard is the product-limit (KM) estimate. Also calculate the life-table (acturial) estimate
ltHaz0_out$hazLT = ltHaz0_out$Events / (ltHaz0_out$AtRisk - ltHaz0_out$Events/2)
# Generate log-time
ltHaz0_out$lnTime <- log(ltHaz0_out$Time)
# For random effects add an ID for each time period
ltHaz0_out$MyId <- 1:dim(ltHaz0_out)[1] # Generate id variable 
# ltHaz0_out$re<-scale(ltHaz0_out$MyId)

# For AR(1) model get outcomes lagged by one.
ltHaz0_out$EventsL <- lag(ltHaz0_out$Events)
# Set first lagged value = 0 (usually would discard, but retain so IC are comparable. Can be justified as a prior value)
ltHaz0_out$EventsL[1] <- 0
#Set surv data
ltHaz0_out$surv <- ltBC_out$Output$S
#timedelta
ltHaz0_out$timedelta<-ltHaz0_out$Time[2]-ltHaz0_out$Time[1]

###------------------------------------------------------------ Process ipi data -----------------------------------------------------------------------------####
###  Process ipi data  ####
## Learn about ipi

table(ipi5y$censrec)
ipi5y %>%
  summarise(Min_surv = min(recyrs, na.rm = TRUE),
            Max_surv = max(recyrs, na.rm = TRUE),
            Mean_surv = mean(recyrs, na.rm = TRUE),
            SD_surv = sd(recyrs, na.rm = TRUE))

table(ipi3y$censrec)
ipi3y %>%
  summarise(Min_surv = min(recyrs, na.rm = TRUE),
            Max_surv = max(recyrs, na.rm = TRUE),
            Mean_surv = mean(recyrs, na.rm = TRUE),
            SD_surv = sd(recyrs, na.rm = TRUE))


##-----ipi data 3Y Monthly life table estimates of hazard-----##
ipi3y$rectime2 <- as.integer(ipi3y$rectime/(365.24/12)) + 1
ipi5y$rectime2 <- as.integer(ipi5y$rectime/(365.24/12)) + 1
#+1 above as integer rounds down, we want to round up
ltBC1 <- lifeTable(ipi3y, timeColumn = "rectime2", eventColumn = "censrec")
ltHaz1 <- data.frame(hazKM = ltBC1$Output$hazard, Time = (seq(1:length(ltBC1$Output[,1]))-0.5)/12,
                     AtRisk = ltBC1$Output$atRisk, Events = ltBC1$Output$events)
# The above hazard is the product-limit (KM) estimate. Also calculate the life-table (acturial) estimate
ltHaz1$hazLT = ltHaz1$Events / (ltHaz1$AtRisk - ltHaz1$Events/2)
# Generate log-time
ltHaz1$lnTime <- log(ltHaz1$Time)
# For random effects add an ID for each time period
ltHaz1$MyId <- 1:dim(ltHaz1)[1] # Generate id variable 
# For AR(1) model get outcomes lagged by one.
ltHaz1$EventsL <- lag(ltHaz1$Events)
# Set first lagged value = 0 (usually would discard, but retain so IC are comparable. Can be justified as a prior value)
ltHaz1$EventsL[1] <- 0
#Set surv data
ltHaz1$surv <- ltBC1$Output$S
#timedelta
ltHaz1$timedelta<-ltHaz1$Time[2]-ltHaz1$Time[1]


##-----ipi data 5Y Monthly life table estimates of hazard----- ##
ltBC1_out <- lifeTable(ipi5y, timeColumn = "rectime2", eventColumn = "censrec")
ltHaz1_out <- data.frame(hazKM = ltBC1_out$Output$hazard, Time = (seq(1:length(ltBC1_out$Output[,1]))-0.5)/12,
                         AtRisk = ltBC1_out$Output$atRisk, Events = ltBC1_out$Output$events)
# The above hazard is the product-limit (KM) estimate. Also calculate the life-table (acturial) estimate
ltHaz1_out$hazLT = ltHaz1_out$Events / (ltHaz1_out$AtRisk - ltHaz1_out$Events/2)
# Generate log-time
ltHaz1_out$lnTime <- log(ltHaz1_out$Time)
# For random effects add an ID for each time period
ltHaz1_out$MyId <- 1:dim(ltHaz1_out)[1] # Generate id variable 
# For AR(1) model get outcomes lagged by one.
ltHaz1_out$EventsL <- lag(ltHaz1_out$Events)
# Set first lagged value = 0 (usually would discard, but retain so IC are comparable. Can be justified as a prior value)
ltHaz1_out$EventsL[1] <- 0
#Set surv data
ltHaz1_out$surv <- ltBC1_out$Output$S
#timedelta
ltHaz1_out$timedelta<-ltHaz1_out$Time[2]-ltHaz1_out$Time[1]
