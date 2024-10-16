library(survival)
library(survminer)

df <- read.csv("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/cleaned_df_with_status_0_or_1.csv")


# Time to failure is -1 in processed data, making it na to fit the model.
df$time_to_failure[df$time_to_failure < 0] <- NA

df$status <- df$had_failure
km_impact <- survfit(Surv(time_to_failure, status) ~ impact_status, data = df)

km_impact_pets <- survfit(Surv(time_to_failure, impact_status) ~ pets, data = df)

km_impact_carpet <- survfit(Surv(time_to_failure, impact_status) ~ carpet_score, data = df)

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
logrank_test_impact
