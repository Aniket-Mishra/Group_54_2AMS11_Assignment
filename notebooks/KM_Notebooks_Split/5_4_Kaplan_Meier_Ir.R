library(survival)
library(survminer)

df <- read.csv("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/cleaned_df_with_status_0_or_1.csv")


# Time to failure is -1 in processed data, making it na to fit the model.
df$time_to_failure[df$time_to_failure < 0] <- NA

df$status <- df$had_failure
km_ir <- survfit(Surv(time_to_failure, status) ~ ir_status, data = df)

km_ir_pets <- survfit(Surv(time_to_failure, ir_status) ~ pets, data = df)

km_ir_carpet <- survfit(Surv(time_to_failure, ir_status) ~ carpet_score, data = df)

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
logrank_test_ir
