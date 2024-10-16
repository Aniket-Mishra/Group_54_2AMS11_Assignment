library(survival)
library(survminer)

n <- 5387
data <- read.csv("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/cleaned_df_with_status_0_or_1.csv")

## CONSIDERING ALL THE SENSORS TOGETHER AND ALL THE FACTORS

y <- data$time_to_failure # Time col
delta <- data$had_failure # Censoring col

surv_object <- Surv(y, delta)
results.km <- survfit(surv_object ~ 1, conf.type = "log-log")

plot(results.km, conf.int = TRUE)

dev.new()
png("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_plot_with_ci_all.png")
plot(results.km, conf.int = TRUE)
dev.off()

summary(results.km)
median(results.km$time[results.km$surv >= 0.5])
