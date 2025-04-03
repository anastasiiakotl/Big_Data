packages <- c("MASS", "car", "readxl", "rgl", "rmarkdown", "nortest",
              "latex2exp", "pca3d", "ISLR", "pls", "corrplot", "glmnet",
              "mvtnorm", "biglm", "leaps", "lme4", "viridis", "ffbase",
              "ks", "KernSmooth", "nor1mix", "np", "locfit",
              "manipulate", "mice", "VIM", "nnet")
install.packages(packages)

# Load packages
lapply(packages, library, character.only = TRUE)

data <- read.table(file = "data/gym_members_exercise_tracking.csv", header = TRUE, sep = ",")

summary(data)
data1 <- data
data1$Gender <- as.factor(data1$Gender)
data1$Workout_Type <- as.factor(data1$Workout_Type)
data1$Experience_Level <- as.factor(data1$Experience_Level)
data1$Workout_Frequency..days.week.<- as.factor(data1$Workout_Frequency..days.week.)

df_male <- subset(data1, Gender == "Male")
df_female <- subset(data1, Gender == "Female")

df_male$Gender <- NULL
df_female$Gender <- NULL

mod_male <- lm(BMI ~ ., data = df_male)
summary(mod_male)

mod_female <- lm(BMI ~ ., data = df_female)
summary(mod_female)

car::vif(mod_male)
car::vif(mod_female)
##########################

df_male$Height_inv2 <- 1 / (df_male$Height..m.^2)
df_male$Avg_BPM2 <- df_male$Avg_BPM^2
df_male$log_Calories <- log(df_male$Calories_Burned)
df_male$Fat_Percentage2 <- df_male$Fat_Percentage^2

m_mod <- lm(BMI ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2, data = df_male)
summary(m_mod)
plot(m_mod, 3)

df_female$Height_inv2 <- 1 / (df_female$Height..m.^2)
df_female$Avg_BPM2 <- df_female$Avg_BPM^2
df_female$log_Calories <- log(df_female$Calories_Burned)
df_female$Fat_Percentage2 <- df_female$Fat_Percentage^2

f_mod <- lm(BMI ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2, data = df_female)
summary(f_mod)
plot(f_mod, 3)
f_mod
car::ncvTest(m_mod)
car::ncvTest(f_mod)

########################## Перетворення 
# log
m_mod_log <- lm(log(BMI) ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2, data = df_male)
plot(m_mod_log, 3)      
car::ncvTest(m_mod_log)   

f_mod_log <- lm(log(BMI) ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2, data = df_female)
plot(f_mod_log, 3)      
car::ncvTest(f_mod_log) 

#Yeo-Johnson
m_yj <- car::powerTransform(BMI ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2, data = df_male, family = "yjPower")
df_male$BMI_yj <- car::bcPower(df_male$BMI, lambda = m_yj$lambda)

m_mod_yj <- lm(BMI_yj ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2, data = df_male)

plot(m_mod_yj, 3)
car::ncvTest(m_mod_yj) 


f_yj <- car::powerTransform(BMI ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2, data = df_female, family = "yjPower")
df_female$BMI_yj <- car::bcPower(df_female$BMI, lambda = f_yj$lambda)

f_mod_yj <- lm(BMI_yj ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2, data = df_female)

plot(f_mod_yj,3)
car::ncvTest(f_mod_yj) 

# sqrt
m_mod_sqrt <- lm(sqrt(BMI) ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2, data = df_male)

plot(m_mod_sqrt, 3)
car::ncvTest(m_mod_sqrt) 


f_mod_sqrt <- lm(sqrt(BMI) ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2, data = df_female)

plot(f_mod_sqrt, 3)
car::ncvTest(f_mod_sqrt) 

#box-cox
library(MASS)
m_mod
m_boxcox_result <- boxcox(m_mod, lambda = seq(-2, 2, 0.1))
m_lambda_opt <- m_boxcox_result$x[which.max(m_boxcox_result$y)]
m_lambda_opt

m_lambda_opt <- 0.5454545
df_male$BMI_boxcox <- (df_male$BMI^m_lambda_opt - 1) / m_lambda_opt
m_mod_boxcox <- lm(BMI_boxcox ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2,
                   data = df_male)
plot(m_mod_boxcox, which = 3)
car::ncvTest(m_mod_boxcox)

f_boxcox_result <- boxcox(f_mod, lambda = seq(-2, 2, 0.1))
f_lambda_opt <- f_boxcox_result$x[which.max(f_boxcox_result$y)]
f_lambda_opt

f_lambda_opt <- 0.4646465
df_female$BMI_boxcox <- (df_female$BMI^f_lambda_opt - 1) / f_lambda_opt
f_mod_boxcox <- lm(BMI_boxcox ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2,
                   data = df_female)
plot(f_mod_boxcox, which = 3)
car::ncvTest(f_mod_boxcox)

###############################################################
plot(m_mod$residuals, type = "o", main = "Residuals over Order", ylab = "Residuals", xlab = "Observation index")

plot(f_mod$residuals, type = "o", main = "Residuals over Order", ylab = "Residuals", xlab = "Observation index")

lag.plot(m_mod$residuals, lags = 1, do.lines = FALSE)
car::durbinWatsonTest(m_mod)

lag.plot(f_mod$residuals, lags = 1, do.lines = FALSE)
car::durbinWatsonTest(f_mod)
##############################################################
df_male$BMI_diff <- c(diff(df_male$BMI), NA)
df_male_diff <- na.omit(df_male)

mod_diff_m <- lm(BMI_diff ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2, 
                 data = df_male_diff)

lag.plot(mod_diff_m$residuals, lags = 1, do.lines = FALSE)
car::durbinWatsonTest(mod_diff_m)


df_female$BMI_diff <- c(diff(df_female$BMI), NA)
df_female_diff <- na.omit(df_female)

mod_diff_f <- lm(BMI_diff ~ Age + Weight..kg. + Height_inv2 + log_Calories + Avg_BPM2 + Fat_Percentage2, 
                 data = df_female_diff)

lag.plot(mod_diff_f$residuals, lags = 1, do.lines = FALSE)
car::durbinWatsonTest(mod_diff_f)
