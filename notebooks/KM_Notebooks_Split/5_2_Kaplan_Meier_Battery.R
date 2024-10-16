library(survival)
library(survminer)

df <- read.csv("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/cleaned_df_with_status_0_or_1.csv")

# Time to failure is -1 in processed data, making it na to fit the model.
df$time_to_failure[df$time_to_failure < 0] <- NA

df$status <- df$had_failure

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
logrank_test_battery
