library(survival)
library(survminer)
library(ggplot2)
path <- "/Users/yukokusakabe/Downloads/lifespan.xlsx"
data <- read_excel(path)
surv_object <- Surv(time = data$lifespan, event = data$event)
fit_haz <- survfit(surv_object ~ 1, data = data) 
ggsurvplot(fit_haz, 
           data=data,
           fun = "cumhaz", # <--- ここで累積ハザードを指定
           xlab = "years",
           ylab = "Cumulative Hazard",
           title = "Cumulative Hazard changed by time",
           ggtheme = theme_bw())