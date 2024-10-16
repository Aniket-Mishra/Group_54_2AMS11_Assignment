library(survival)
library(survminer)

df <- read.csv("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/cleaned_df_with_status_0_or_1.csv")

# Time to failure is -1 in processed data, making it 1825 (days in study) for cox model. Others needed na.
df$time_to_failure[df$time_to_failure < 0] <- 1825

sum_na <- colSums(is.na(df[c("time_to_failure", "had_failure", "pets", "carpet_score")]))
summary_cox <- summary(df[c("time_to_failure", "had_failure", "pets", "carpet_score")])

print("All Na Values in important cols:")
print(sum_na)
print("Summary of data:")
print(summary_cox)

surv_object <- Surv(df$time_to_failure, df$had_failure)
cox_model <- coxph(surv_object ~ pets + carpet_score, data = df)

summary_cox_model <- summary(cox_model)
print("Cox model summary:")
print(summary_cox_model)

km_plot <- ggsurvplot(survfit(cox_model),
    data = df,
    risk.table = TRUE,
    title = "COX Failure for all"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/cox_curve_all.png", km_plot$plot)

# For all battery:
battery_data <- df[df$battery_status == 1, ]
cox_battery <- coxph(
    surv_object_battery ~ pets + carpet_score,
    data = battery_data
)
cox_battery_pets <- coxph(
    surv_object_battery ~ pets,
    data = battery_data
)
cox_battery_carpets <- coxph(
    surv_object_battery ~ carpet_score,
    data = battery_data
)

print(summary(cox_battery))
print(summary(cox_battery_pets))
print(summary(cox_battery_carpets))

km_plot <- ggsurvplot(survfit(cox_battery),
    data = battery_data,
    risk.table = TRUE,
    title = "COX battery Failure for all"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/cox_curve_battery_pets.png", km_plot$plot)

km_plot <- ggsurvplot(survfit(cox_battery_pets),
    data = battery_data,
    risk.table = TRUE,
    title = "COX battery Failure for Pets"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/cox_curve_battery_pets.png", km_plot$plot)

km_plot <- ggsurvplot(survfit(cox_battery_carpets),
    data = battery_data,
    risk.table = TRUE,
    title = "COX battery Failure for Carpets"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/cox_curve_battery_carpet.png", km_plot$plot)

# For All Impact

impact_data <- df[df$impact_status == 1, ]

surv_object_impact <- Surv(time = impact_data$time_to_failure, event = impact_data$had_failure)

cox_impact <- coxph(surv_object_impact ~ pets + carpet_score, data = impact_data)
cox_impact_pets <- coxph(surv_object_impact ~ pets, data = impact_data)
cox_impact_carpet <- coxph(surv_object_impact ~ carpet_score, data = impact_data)

print(summary(cox_ir))
print(summary(cox_ir_pets))
print(summary(cox_ir_carpet))

km_plot <- ggsurvplot(survfit(cox_impact),
    data = impact_data,
    title = "COX Impact Failure for Pets and Carpets",
    risk.table = TRUE
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/cox_curve_impact_pets_carpet.png", km_plot$plot)

km_plot <- ggsurvplot(survfit(cox_impact),
    data = impact_data,
    title = "COX Impact Failure for Pets",
    risk.table = TRUE
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/cox_curve_impact_pets.png", km_plot$plot)

km_plot <- ggsurvplot(survfit(cox_impact),
    data = impact_data,
    title = "COX Impact Failure for Carpets",
    risk.table = TRUE
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/cox_curve_impact_carpet.png", km_plot$plot)

# For all IR
ir_data <- df[df$ir_status == 1, ]
surv_object_ir <- Surv(time = ir_data$time_to_failure, event = ir_data$had_failure)

cox_ir <- coxph(surv_object_ir ~ pets + carpet_score,
    data = ir_data
)
cox_ir_pets <- coxph(surv_object_ir ~ pets,
    data = ir_data
)
cox_ir_carpet <- coxph(surv_object_ir ~ carpet_score,
    data = ir_data
)
print(summary(cox_ir))
print(summary(cox_ir_pets))
print(summary(cox_ir_carpet))

km_plot <- ggsurvplot(survfit(cox_ir),
    data = ir_data,
    risk.table = TRUE,
    title = "COX IR Failure for Pets and Carpets"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/cox_curve_ir_pets_carpet.png", km_plot$plot)

km_plot <- ggsurvplot(survfit(cox_ir_pets),
    data = ir_data,
    risk.table = TRUE,
    title = "COX IR Failure for Pets"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/cox_curve_ir_pets.png", km_plot$plot)

km_plot <- ggsurvplot(survfit(cox_ir_carpet),
    data = ir_data,
    risk.table = TRUE,
    title = "COX IR Failure for Carpets"
)
ggsave("/Users/paniket/TU_Eindhoven/2_Study/Q1_2AMS11_survival_Statistics_for_Data_Scientists/2_Assignments/group/Group_54_2AMS11_Assignment/output/plots/cox_curve_ir_carpet.png", km_plot$plot)
