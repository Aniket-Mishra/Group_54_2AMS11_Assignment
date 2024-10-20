
library(survival)
library(survminer)
library(ggplot2)


df <- read.csv("~/Downloads/cleaned_df_with_status_0_or_1.csv")

df$time_to_failure[df$time_to_failure < 0] <- NA  
df$status <- ifelse(df$had_failure == 1, 1, 0)


df <- df[!is.na(df$time_to_failure) & !is.na(df$status), ]



battery_data <- df[df$battery_status == 1, ] 
impact_data <- df[df$impact_status == 1, ]
ir_data <- df[df$ir_status == 1, ]


df$carpet_score_group <- cut(df$carpet_score, breaks = 0:8, include.lowest = TRUE, labels = FALSE)
df$total_usage_time_group <- cut(df$total_usage_time, breaks = seq(0, 5000, by = 500), include.lowest = TRUE)


km_carpet <- survfit(Surv(time_to_failure, status) ~ carpet_score_group, data = df)
weibull_carpet <- survreg(Surv(time_to_failure, status) ~ carpet_score_group, data = df, dist = "weibull")
exp_carpet <- survreg(Surv(time_to_failure, status) ~ carpet_score_group, data = df, dist = "exponential")

#for total usage time
km_usage <- survfit(Surv(time_to_failure, status) ~ total_usage_time_group, data = df)
weibull_usage <- survreg(Surv(time_to_failure, status) ~ total_usage_time_group, data = df, dist = "weibull")
exp_usage <- survreg(Surv(time_to_failure, status) ~ total_usage_time_group, data = df, dist = "exponential")


#For BATTERY

impact_data$carpet_score_group <- cut(impact_data$carpet_score, breaks = 0:8, include.lowest = TRUE, labels = FALSE)
impact_data$total_usage_time_group <- cut(impact_data$total_usage_time, breaks = seq(0, 5000, by = 500), include.lowest = TRUE)

#Kaplan-Meier, Weibull, and Exponential for Carpet Score 
km_carpet_impact <- survfit(Surv(time_to_failure, status) ~ carpet_score_group, data = impact_data)
weibull_carpet_impact <- survreg(Surv(time_to_failure, status) ~ carpet_score_group, data = impact_data, dist = "weibull")
exp_carpet_impact <- survreg(Surv(time_to_failure, status) ~ carpet_score_group, data = impact_data, dist = "exponential")

# Kaplan-Meier, Weibull, and Exponential for Total Usage Time 
km_usage_impact <- survfit(Surv(time_to_failure, status) ~ total_usage_time_group, data = impact_data)
weibull_usage_impact <- survreg(Surv(time_to_failure, status) ~ total_usage_time_group, data = impact_data, dist = "weibull")
exp_usage_impact <- survreg(Surv(time_to_failure, status) ~ total_usage_time_group, data = impact_data, dist = "exponential")

###Kaplan-Meier, Weibull, and Exponential for Pets
km_pets_impact <- survfit(Surv(time_to_failure, status) ~ pets, data = impact_data)
weibull_pets_impact <- survreg(Surv(time_to_failure, status) ~ pets, data = impact_data, dist = "weibull")
exp_pets_impact <- survreg(Surv(time_to_failure, status) ~ pets, data = impact_data, dist = "exponential")



pdf("Km_param_ir_all.pdf", width = 10, height = 10)


plot_survival_curves <- function(km_fit, factor_name, weibull_fit, exp_fit, time_points) {
  

  km_plot <- ggsurvplot(km_fit, conf.int = FALSE, ggtheme = theme_minimal(), palette = "Dark2",
                        title = paste("Survival Curves for", factor_name))$plot
  

  weibull_curve <- data.frame(
    time = time_points,
    survival = 1 - pweibull(time_points, shape = 1 / weibull_fit$scale, scale = exp(coef(weibull_fit)[1])),
    curve_type = "Weibull"
  )
  
  exp_curve <- data.frame(
    time = time_points,
    survival = 1 - pexp(time_points, rate = 1 / exp(coef(exp_fit)[1])),
    curve_type = "Exponential"
  )
  

  km_plot <- km_plot +
    geom_line(data = weibull_curve, aes(x = time, y = survival, color = curve_type), linetype = "dashed", linewidth = 1) +
    geom_line(data = exp_curve, aes(x = time, y = survival, color = curve_type), linetype = "dotted", linewidth = 1) +
    scale_color_manual(name = "Curve Type", values = c("Weibull" = "blue", "Exponential" = "red", "Kaplan-Meier" = "black")) +
    labs(color = "Curve Type") +
    theme(legend.position = "bottom")
  

  print(km_plot)
}

time_points <- seq(0, max(df$time_to_failure, na.rm = TRUE), by = 0.1)


plot_survival_curves(km_carpet_impact, "Carpet Score For impact", weibull_carpet_impact, exp_carpet_impact, time_points)


plot_survival_curves(km_usage_impact, "Total Usage Time for impact", weibull_usage_impact, exp_usage_impact, time_points)



plot_survival_curves(km_pets_impact, "Pets for impact", weibull_pets_impact, exp_pets_impact, time_points)

dev.off()