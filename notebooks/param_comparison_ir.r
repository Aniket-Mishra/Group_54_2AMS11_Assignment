# Load necessary libraries
library(survival)
library(survminer)
library(ggplot2)


df <- read.csv("~/Downloads/cleaned_df_with_status_0_or_1.csv")


df$time_to_failure[df$time_to_failure < 0] <- NA  
df$status <- ifelse(df$had_failure == 1, 1, 0)


df <- df[!is.na(df$time_to_failure) & !is.na(df$status), ]

# Subset the data for IR sensors
ir_data <- df[df$ir_status == 1, ]


ir_data$carpet_score_group <- cut(ir_data$carpet_score, breaks = 0:8, include.lowest = TRUE, labels = FALSE)

ir_data <- ir_data[!is.na(ir_data$time_to_failure) & ir_data$time_to_failure > 0, ]


surv_object <- Surv(time = ir_data$time_to_failure, event = ir_data$status)


weibull_model_combined <- survreg(surv_object ~ pets + carpet_score_group, data = ir_data, dist = "weibull")


scale_weibull <- exp(coef(weibull_model_combined)[1])  
shape_weibull <- 1 / weibull_model_combined$scale      # Shape (beta)




time_points <- seq(0, max(ir_data$time_to_failure, na.rm = TRUE), by = 0.1)


plot_survival_curves_combined <- function(km_fit, weibull_fit, factor_name, time_points) {
  
#kaplan meir
  km_plot <- ggsurvplot(km_fit, conf.int = FALSE, ggtheme = theme_minimal(), palette = "Dark2",
                        title = paste("Survival Curves for", factor_name))$plot
  
  # Weibull curve data
  weibull_curve <- data.frame(
    time = time_points,
    survival = 1 - pweibull(time_points, shape = 1 / weibull_fit$scale, scale = exp(coef(weibull_fit)[1])),
    curve_type = "Weibull"
  )
  
  km_plot <- km_plot +
    geom_line(data = weibull_curve, aes(x = time, y = survival, color = curve_type), linetype = "dashed", linewidth = 1) +
    scale_color_manual(name = "Curve Type", values = c("Weibull" = "blue", "Kaplan-Meier" = "black")) +
    labs(color = "Curve Type") +
    theme(legend.position = "bottom")
  

  print(km_plot)
}

# Kaplan-Meier model for combined pets and carpet score
km_combined <- survfit(Surv(time_to_failure, status) ~ pets + carpet_score_group, data = ir_data)

pdf("Km_param_ir_combined.pdf", width = 10, height = 10)
plot_survival_curves_combined(km_combined, weibull_model_combined, "Pets + Carpet Score for IR", time_points)
dev.off()
