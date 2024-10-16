#### The KM Estimator

library(survival)
library(survminer)


n <- 5387

#reading the csv
data <- read.csv("~/Downloads/data_cleaned.csv")


##CONSIDERING ALL THE SENSORS TOGETHER AND ALL THE FACTORS

# Step 2: Identify columns for time and censoring indicator
# and the censoring status (1 for event, 0 for censored) is in a column named "status"

y <- data$time_to_failure         # The observed time column
delta <- data$had_failure    # The censoring indicator column

# Step 3: Create a survival object
surv_object <- Surv(y, delta)

# Step 4: Fit a Kaplan-Meier model (or any other survival model)
results.km <- survfit(surv_object ~ 1, conf.type = "log-log")

# Step 5: Plot the Kaplan-Meier survival curve
plot(results.km, conf.int = TRUE)

dev.new()
# Save the plot to a PNG file
png("km_plot_with_ci_all.png")  # Opens the PNG device
plot(results.km, conf.int = TRUE)  # Create the plot
dev.off()  # Close the device and save the file

#summary(results.km)
median(results.km$time[results.km$surv >= 0.5])


#For all the parts individually

df <- read.csv("~/Downloads/cleaned_df_with_status_0_or_1.csv")


# Replace negative values (-1) in time_to_failure with NA or an appropriate censoring time
df$time_to_failure[df$time_to_failure < 0] <- NA  

# Convert had_failure column to a binary status (1 = failure, 0 = censored)
df$status <- df$had_failure

# Fit Kaplan-Meier curve for each sensor type (battery, impact, ir)
km_battery <- survfit(Surv(time_to_failure, status) ~ battery_status, data = df)
km_impact <- survfit(Surv(time_to_failure, status) ~ impact_status, data = df)
km_ir <- survfit(Surv(time_to_failure, status) ~ ir_status, data = df)

#Fit kaplan-meir for battery against pets

km_battery_pets <- survfit(Surv(time_to_failure, battery_status) ~ pets, data = df)

#Fit kaplan-meir for battery against carpet

km_battery_carpet <- survfit(Surv(time_to_failure, battery_status) ~ carpet_score, data = df)

#Fit Kaplan Meir for impact against pets

km_impact_pets <- survfit(Surv(time_to_failure, impact_status) ~ pets, data = df)

#Fit Kaplan Meir for impact against carpet

km_impact_carpet <- survfit(Surv(time_to_failure, impact_status) ~ carpet_score, data = df)

#Fit Kaplan Meir for IR against pets 

km_ir_pets <- survfit(Surv(time_to_failure, ir_status) ~ pets, data = df)

#Fit Kaplan Meir for IR against carpet 

km_ir_carpet <- survfit(Surv(time_to_failure, ir_status) ~ carpet_score, data = df)

# Open a PDF file to save the plots
pdf("Kaplan_Meier_Sensor_Plots_new_new.pdf", width = 10, height = 10)

# Plot for Battery Status
ggsurvplot(km_battery, conf.int = TRUE, 
           title = "Kaplan-Meier Curve for Battery Failure",
           xlab = "Time (Total Cleaning Time)",
           ylab = "Survival Probability")

# Plot for Impact Status
ggsurvplot(km_impact, conf.int = TRUE, 
           title = "Kaplan-Meier Curve for Impact Failure",
           xlab = "Time (Total Cleaning Time)",
           ylab = "Survival Probability")

# Plot for IR Status
ggsurvplot(km_ir, conf.int = TRUE, 
           title = "Kaplan-Meier Curve for IR Failure",
           xlab = "Time (Total Cleaning Time)",
           ylab = "Survival Probability")

#Plot for Battery against pets 

ggsurvplot(km_battery_pets, conf.int = TRUE, 
           title = "Kaplan-Meier Curve for Battery Failure against pets",
           xlab = "Time (Total Cleaning Time)",
           ylab = "Survival Probability")

#Plot for Battery against carpet 

ggsurvplot(km_battery_carpet, conf.int = FALSE, 
           title = "Kaplan-Meier Curve for Battery Failure against Carpet score",
           xlab = "Time (Total Cleaning Time)",
           ylab = "Survival Probability")


#Plot for Impact against pets 

ggsurvplot(km_impact_pets, conf.int = TRUE, 
           title = "Kaplan-Meier Curve for Impact Failure against pets",
           xlab = "Time (Total Cleaning Time)",
           ylab = "Survival Probability")

#Plot for Impact against carpet

ggsurvplot(km_impact_carpet, conf.int = FALSE, 
           title = "Kaplan-Meier Curve for Impact Failure against Carpet score",
           xlab = "Time (Total Cleaning Time)",
           ylab = "Survival Probability")

#Plot for IR against pets 

ggsurvplot(km_ir_pets, conf.int = TRUE, 
           title = "Kaplan-Meier Curve for IR Failure against pets",
           xlab = "Time (Total Cleaning Time)",
           ylab = "Survival Probability")

#Plot for IR against carpets 

ggsurvplot(km_ir_carpet, conf.int = FALSE, 
           title = "Kaplan-Meier Curve for IR Failure against carpet score",
           xlab = "Time (Total Cleaning Time)",
           ylab = "Survival Probability")


# Perform log-rank test to compare survival curves
logrank_test_battery <- survdiff(Surv(y,delta) ~ battery_status , data = df)
logrank_test_impact <- survdiff(Surv(y,delta) ~ impact_status , data = df)
logrank_test_ir <- survdiff(Surv(y,delta) ~ ir_status , data = df)
# Print the results
logrank_test_battery
logrank_test_impact
logrank_test_ir



#USING COX TO SEE THE IMPACT OF FACTORS ON PARTS
# Replace negative values (-1) in time_to_failure with NA or an appropriate censoring time
df$time_to_failure[is.na(df$time_to_failure)] <- 1800  # Use is.na() to find NA values

# Check for NA values explicitly
sum(is.na(df$time_to_failure))  # Should return 0 if there are no NA values



summary(df[c("time_to_failure", "had_failure", "pets", "carpet_score")])

# Check for missing values in the relevant columns
colSums(is.na(df[c("time_to_failure", "had_failure", "pets", "carpet_score")]))

surv_object <- Surv(df$time_to_failure, df$had_failure)
cox_model <- coxph(surv_object ~ pets + carpet_score, data = df)
summary(cox_model)

# Fit a Cox model for Battery

# Subset the data to include only rows where battery failed
battery_data <- df[df$battery_status == 1, ]

# Create the survival object for battery failures
surv_object_battery <- Surv(time = battery_data$time_to_failure, event = battery_data$had_failure)

#For both pets and carpets
cox_battery <- coxph(surv_object_battery ~ pets + carpet_score, data = battery_data)
summary(cox_battery)
# Generate survival curves based on the Cox model
ggsurvplot(survfit(cox_battery), data = battery_data, 
                    risk.table = TRUE,
                    title = "COX battery Failure for Pets + Carpets")


#For just pets
cox_battery_pets <- coxph(surv_object_battery ~ pets, data = battery_data)
summary(cox_battery_pets)
# Generate survival curves based on the Cox model
ggsurvplot(survfit(cox_battery_pets), data = battery_data, 
           risk.table = TRUE,
           title = "COX battery Failure for Pets")


#For just carpets

cox_battery_carpets <- coxph(surv_object_battery ~ carpet_score, data = battery_data)
summary(cox_battery_carpets)
# Generate survival curves based on the Cox model
ggsurvplot(survfit(cox_battery_carpets), data = battery_data, 
           risk.table = TRUE,
           title = "COX battery Failure for Carpets")


# Fit a Cox model for IR Sensor

# Subset the data to include only rows where battery failed
ir_data <- df[df$ir_status == 1, ]

# Create the survival object for battery failures
surv_object_ir <- Surv(time = ir_data$time_to_failure, event = ir_data$had_failure)

#For both pets and carpets
cox_ir <- coxph(surv_object_ir ~ pets + carpet_score, data = ir_data)
summary(cox_ir)
# Generate survival curves based on the Cox model
ggsurvplot(survfit(cox_ir), data = ir_data, 
                    risk.table = TRUE, 
                    title = "COX IR Failure for Pets and Carpets")

#For pets
cox_ir <- coxph(surv_object_ir ~ pets , data = ir_data)
summary(cox_ir)
# Generate survival curves based on the Cox model
ggsurvplot(survfit(cox_ir), data = ir_data, 
           risk.table = TRUE, 
           title = "COX IR Failure for Pets")

#For carpets
cox_ir <- coxph(surv_object_ir ~ carpet_score, data = ir_data)
summary(cox_ir)
# Generate survival curves based on the Cox model
ggsurvplot(survfit(cox_ir), data = ir_data, 
           risk.table = TRUE, 
           title = "COX IR Failure for Carpets")


# Fit a Cox model for Impact Sensor

# Subset the data to include only rows where battery failed
impact_data <- df[df$impact_status == 1, ]

# Create the survival object for battery failures
surv_object_impact <- Surv(time = impact_data$time_to_failure, event = impact_data$had_failure)

cox_impact <- coxph(surv_object_impact ~ pets + carpet_score, data = impact_data)
summary(cox_impact)
# Generate survival curves based on the Cox model
ggsurvplot(survfit(cox_impact), data = impact_data,
                    title = "COX Impact Failure for Pets and Carpets",
                    risk.table = TRUE)

cox_impact <- coxph(surv_object_impact ~ pets , data = impact_data)
summary(cox_impact)
# Generate survival curves based on the Cox model
ggsurvplot(survfit(cox_impact), data = impact_data,
           title = "COX Impact Failure for Pets",
           risk.table = TRUE)

cox_impact <- coxph(surv_object_impact ~ carpet_score, data = impact_data)
summary(cox_impact)
# Generate survival curves based on the Cox model
ggsurvplot(survfit(cox_impact), data = impact_data,
           title = "COX Impact Failure for Carpets",
           risk.table = TRUE)


# Close the PDF file
dev.off()









