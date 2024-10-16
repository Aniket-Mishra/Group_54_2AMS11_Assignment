library(survival)
library(survminer)

n <- 5387
df <- read.csv("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/cleaned_df_with_status_0_or_1.csv")

## CONSIDERING ALL THE SENSORS TOGETHER AND ALL THE FACTORS

y <- df$time_to_failure # Time col
delta <- df$had_failure # Censoring col

surv_object <- Surv(y, delta)
results.km <- survfit(surv_object ~ 1, conf.type = "log-log")

plot(results.km, conf.int = TRUE)

dev.new()
png("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_plot_with_ci_all.png")
plot(results.km, conf.int = TRUE)
dev.off()

summary <- summary(results.km)
print("KM All Summary:")
print(summary)
print("KM All Median:")
print(median(results.km$time[results.km$surv >= 0.5]))

df$status <- df$had_failure

# KM Battery - Everything here rn

km_battery <- survfit(Surv(time_to_failure, status) ~ battery_status, data = df)
km_battery_pets <- survfit(
    Surv(time_to_failure, battery_status)
    ~ pets,
    data = df
)
km_battery_carpet <- survfit(
    Surv(time_to_failure, battery_status)
    ~ carpet_score,
    data = df
)

print(summary(km_battery))
print(summary(km_battery_pets))
print(summary(ckm_battery_carpet))

km_plot <- ggsurvplot(km_battery,
    conf.int = TRUE,
    title = "Kaplan-Meier Curve for Battery Failure",
    xlab = "Time (Total Cleaning Time)",
    ylab = "Survival Probability"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_curve_battery.png", km_plot$plot)

km_plot <- ggsurvplot(km_battery_pets,
    conf.int = TRUE,
    title = "Kaplan-Meier Curve for Battery Failure against pets",
    xlab = "Time (Total Cleaning Time)",
    ylab = "Survival Probability"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_curve_battery_pets.png", km_plot$plot)

km_plot <- ggsurvplot(km_battery_carpet,
    conf.int = FALSE,
    title = "Kaplan-Meier Curve for Battery Failure against Carpet score",
    xlab = "Time (Total Cleaning Time)",
    ylab = "Survival Probability"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_curve_battery_carpet.png", km_plot$plot)

logrank_test_battery <- survdiff(Surv(y, delta) ~ battery_status, data = df)
p_value <- 1 - pchisq(logrank_test_battery$chisq, length(logrank_test_battery$n) - 1)

km_fit <- survfit(Surv(y, delta) ~ battery_status, data = df)
km_plot <- ggsurvplot(
    km_fit,
    conf.int = TRUE,
    pval = paste("p =", round(p_value, 4)),
    risk.table = TRUE,
    title = "Log Rank Curves by Battery Status",
    xlab = "Time (Total Cleaning Time)",
    ylab = "Survival Probability",
    legend.title = "Battery Status",
)

print(km_plot)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_curve_battery_log_rank_test.png", km_plot$plot)

print(logrank_test_impact)

# KM Impact - Everything here rn

km_impact <- survfit(Surv(time_to_failure, status) ~ impact_status, data = df)
km_impact_pets <- survfit(
    Surv(time_to_failure, impact_status)
    ~ pets,
    data = df
)
km_impact_carpet <- survfit(
    Surv(time_to_failure, impact_status)
    ~ carpet_score,
    data = df
)

print(summary(km_impact))
print(summary(km_impact_pets))
print(summary(ckm_impact_carpet))

km_plot <- ggsurvplot(km_impact,
    conf.int = TRUE,
    title = "Kaplan-Meier Curve for Impact Failure against pets",
    xlab = "Time (Total Cleaning Time)",
    ylab = "Survival Probability"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_curve_impact.png", km_plot$plot)

km_plot <- ggsurvplot(km_impact_pets,
    conf.int = TRUE,
    title = "Kaplan-Meier Curve for Impact Failure against pets",
    xlab = "Time (Total Cleaning Time)",
    ylab = "Survival Probability"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_curve_impact_pets.png", km_plot$plot)

km_plot <- ggsurvplot(km_impact_carpet,
    conf.int = FALSE,
    title = "Kaplan-Meier Curve for Impact Failure against Carpet score",
    xlab = "Time (Total Cleaning Time)",
    ylab = "Survival Probability"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_curve_impact_carpet.png", km_plot$plot)

logrank_test_impact <- survdiff(Surv(y, delta) ~ impact_status, data = df)
p_value <- 1 - pchisq(logrank_test_impact$chisq, length(logrank_test_impact$n) - 1)

km_fit <- survfit(Surv(y, delta) ~ impact_status, data = df)
km_plot <- ggsurvplot(
    km_fit,
    conf.int = TRUE,
    pval = paste("p =", round(p_value, 4)),
    risk.table = TRUE,
    title = "Log Rank Curves by Impact Status",
    xlab = "Time (Total Cleaning Time)",
    ylab = "Survival Probability",
    legend.title = "Impact Status",
)

print(km_plot)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_curve_impact_log_rank_test.png", km_plot$plot)

print(logrank_test_impact)

# IR For everything rn

km_ir <- survfit(Surv(time_to_failure, status) ~ ir_status, data = df)
km_ir_pets <- survfit(
    Surv(time_to_failure, ir_status)
    ~ pets,
    data = df
)
km_ir_carpet <- survfit(
    Surv(time_to_failure, ir_status)
    ~ carpet_score,
    data = df
)

print(summary(km_ir))
print(summary(km_ir_pets))
print(summary(ckm_ir_carpet))

km_plot <- ggsurvplot(km_ir,
    conf.int = TRUE,
    title = "Kaplan-Meier Curve for ir Failure against pets",
    xlab = "Time (Total Cleaning Time)",
    ylab = "Survival Probability"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_curve_ir.png", km_plot$plot)

km_plot <- ggsurvplot(km_ir_pets,
    conf.int = TRUE,
    title = "Kaplan-Meier Curve for ir Failure against pets",
    xlab = "Time (Total Cleaning Time)",
    ylab = "Survival Probability"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_curve_ir_pets.png", km_plot$plot)

km_plot <- ggsurvplot(km_ir_carpet,
    conf.int = FALSE,
    title = "Kaplan-Meier Curve for ir Failure against Carpet score",
    xlab = "Time (Total Cleaning Time)",
    ylab = "Survival Probability"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_curve_ir_carpet.png", km_plot$plot)

logrank_test_ir <- survdiff(Surv(y, delta) ~ ir_status, data = df)
p_value <- 1 - pchisq(logrank_test_ir$chisq, length(logrank_test_ir$n) - 1)

km_fit <- survfit(Surv(y, delta) ~ impact_status, data = df)
km_plot <- ggsurvplot(
    km_fit,
    conf.int = TRUE,
    pval = paste("p =", round(p_value, 4)),
    risk.table = TRUE,
    title = "Log Rank Curves by IR Status",
    xlab = "Time (Total Cleaning Time)",
    ylab = "Survival Probability",
    legend.title = "IR Status",
)

print(km_plot)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/km_curve_ir_log_rank_test.png", km_plot$plot)

print(logrank_test_ir)
