install.packages("survival")
install.packages("survminer")
install.packages("readxl")
library(readxl)
library(survival)
library(survminer)
data <- read.xlsx("/Users/yukokusakabe/Downloads/Book3.xlsx")
colnames(data) <- c("Bridge_ID", "lifespan", "group", "event")
data$group <- as.factor(data$group)
surv_object <- Surv(time = data$lifespan, event = data$event)

library(readxl)
library(survival)
library(survminer)
path <- "/Users/yukokusakabe/Downloads/Book3.xlsx"
data <- read_excel(path)

data$group<-as.factor(data$group)
surv_object<-Surv(time=data$lifespan,event=data$event)
fit_km<-survfit(surv_object~group, data=data)
ggsurvplot(
  fit_km,
  data=data,
  pval=TRUE,
  conf.int=TRUE,
  risk.table=TRUE,
  risk.table.col="strata",
  legend.title="Bridges",
  xlab="yeas",
  ylab="probability",
  title="gousei vs. higousei",
  ggtheme=theme_bw()
)
