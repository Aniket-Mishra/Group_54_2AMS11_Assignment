library(survival)


df <- read.csv("~/Downloads/cleaned_df_with_status_0_or_1.csv")


df$time_to_failure[df$time_to_failure < 0] <- NA  
df$status <- ifelse(df$had_failure == 1, 1, 0)


df <- df[!is.na(df$time_to_failure) & !is.na(df$status), ]


ir_data <- df[df$ir_status == 1, ]

ir_data_filtered <- ir_data[!is.na(ir_data$time_to_failure) & ir_data$time_to_failure > 0, ]


surv_object <- Surv(time = ir_data_filtered$time_to_failure, event = ir_data_filtered$status)


weibull_model <- survreg(surv_object ~ 1, dist = "weibull")


scale_weibull <- exp(weibull_model$coefficients)  
shape_weibull <- 1 / weibull_model$scale      #beta

#L10 value
L10 <- 1943 * (-log(0.9))^(1 / 3.995)
print(L10)

# Check if L10 meets the manufacturer's guarantee of 2000 days
if (L10 >= 2000) {
  print("The IR sensor meets the manufacturer's L10 guarantee of 2000 days.")
} else {
  print("The IR sensor does not meet the manufacturer's L10 guarantee")
}
