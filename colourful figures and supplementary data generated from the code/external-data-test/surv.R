#library
library(survHE)
library(survival)
library(survminer)

###整理文件
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")

#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
# #对照组个体数据
# intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
# #合并后两个个体数据
# data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=intervation1)
fit
ggsurvplot(fit,data=intervation1,pval = TRUE,risk.table = TRUE,
           palette = "#D95F02",
           legend.tittle="干预措施",
           legend.labs="ipi",
           risk.table.height=0.2,
           xlab=c("生存时间（月）"),
           break.x.by=12,
           break.y.by=0.2,
           xlim=c(0,120),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())