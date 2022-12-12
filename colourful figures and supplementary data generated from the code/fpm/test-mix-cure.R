haz_rate = function(x, t, out = "prob"){ 
  tmp  = t - lag(t, default = 0)
  if (out == "rate"){
    y = case_when(x == 1 ~ 1, TRUE ~ - (log(1 - x)) / tmp)
  } else if (out == "prob") {
    y = 1 - exp(- x * tmp)
  } else {
    "error!"
  }
  return (y)
}


# England 2016  # should be changed
Eng_HMD = read.table(here("USA_LF.txt"), header=TRUE)
USA_HMD = as.data.frame(matrix(ncol = 5,nrow = 84))
colnames(USA_HMD)<-c("Age","qx","lx","dx","Year")
USA_HMD$Age<-seq(1,84,1)
for (i in 1:4) {
  USA_HMD$qx[i]<-Eng_HMD$qx[1]
  USA_HMD$lx[i]<-Eng_HMD$lx[1]
  USA_HMD$dx[i]<-Eng_HMD$dx[1]
}
for (i in 2:17) {
  for (j in ((i-1)*5):(i*5-1)) {
    USA_HMD$qx[j]<-Eng_HMD$qx[i]
    USA_HMD$lx[j]<-Eng_HMD$lx[i]
    USA_HMD$dx[j]<-Eng_HMD$dx[i]  
  }
}
USA_HMD$Year<-2015

USA_2015<-as.data.frame(matrix(ncol = 4,nrow = 84))
colnames(USA_2015)<-c("Age","Hazard","Surv","Year")
USA_2015$Age<-USA_HMD$Age
USA_2015$Hazard[1:4]<-haz_rate(USA_HMD$qx[1:4], 4, "rate")
USA_2015$Hazard[5:84]<-haz_rate(USA_HMD$qx[5:84], 5, "rate")
USA_2015$Surv<-(USA_HMD$lx-USA_HMD$dx)/USA_HMD$lx
USA_2015$Year<-USA_2015$Age-62
tmp = select(USA_2015, Year, Age, Hazard)

Eng_2016 = filter(Eng_HMD, Year == 2015) %>% mutate(tmp = Age,
                                                    Age = as.numeric(tmp)[tmp],
                                                    Hazard = haz_rate(qx, 5, "rate"),  # All observations 1 unit apart
                                                    Surv = (lx - dx) / lx[1],
                                                    Years = Age - 62)
tmp = select(Eng_2016, Year, Age, Hazard)






