# Installation of required packages
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
data1
data1$Workout_Type <- as.factor(data1$Workout_Type)
data1$Experience_Level <- as.factor(data1$Experience_Level)
data1$Workout_Frequency..days.week.<- as.factor(data1$Workout_Frequency..days.week.)

df_male <- subset(data1, Gender == "Male")
df_female <- subset(data1, Gender == "Female")

df_male
df_female

df_male$Gender <- NULL
df_female$Gender <- NULL

# Попередня оцінка

summary(df_male)  # Для чоловіків
summary(df_female)  # Для жінок

# Дiаграми розсiювання та кореляцiйні матриці для чоловіків
numeric_vars_male <- df_male[sapply(df_male, is.numeric)]

car::scatterplotMatrix(numeric_vars_male,
                       col = 1,
                       regLine = list(col = 2),
                       smooth = list(col.smooth = 4, col.spread = 4),
                       main = "Scatterplot Matrix — Чоловіки")

# Дiаграми розсiювання та кореляцiйні матриці для жінок
# Матриця для жінок (тільки числові змінні)
numeric_vars_female <- df_female[sapply(df_female, is.numeric)]

car::scatterplotMatrix(numeric_vars_female,
                       col = 1,
                       regLine = list(col = 2),
                       smooth = list(col.smooth = 4, col.spread = 4),
                       main = "Scatterplot Matrix — Жінки")

# task1

# Модель для чоловіків
# Age + Weight..kg. + Height..m. + Avg_BPM + Calories_Burned
mod_male <- lm(BMI ~ ., data = df_male)
mod_male_v2 <- lm(BMI ~ Age + Weight..kg. + Height..m. + Avg_BPM + Calories_Burned, data = df_male)
summary(mod_male_v2)
summary(mod_male)

# Для чоловіків
df_male$Height_inv2 <- 1 / (df_male$Height..m.^2)
df_male$Avg_BPM2 <- df_male$Avg_BPM^2
df_male$log_Calories <- log(df_male$Calories_Burned)
df_male$Fat_Percentage2 <- df_male$Fat_Percentage^2


mod_male_v3 <- lm(BMI ~ Age + Weight..kg. + Height_inv2 + Avg_BPM2 + log_Calories, data = df_male)
summary(mod_male_v3)

mod_male_v4 <- lm(BMI ~ Age + Weight..kg. + Height_inv2 + log_Calories + Fat_Percentage2, data = df_male)
summary(mod_male_v4)


# Модель для жінок

df_female$Height_inv2 <- 1 / (df_female$Height..m.^2)
df_female$Avg_BPM2 <- df_female$Avg_BPM^2
df_female$log_Calories <- log(df_female$Calories_Burned)
df_female$Fat_Percentage2 <- df_female$Fat_Percentage^2

mod_female <- lm(BMI ~ ., data = df_female)
summary(mod_female)
# mod_female_v2 <- lm(BMI ~ Age + Weight..kg. + Height_inv2 + log_Calories + Fat_Percentage2, data = df_female)
mod_female_v2 <- lm(BMI ~ Age + Weight..kg. + Height_inv2 + Avg_BPM2 + log_Calories + Fat_Percentage2, data = df_female)
summary(mod_female_v2)

#mod_female_v3 <- lm(BMI ~ Age + Weight..kg. + Height_inv2 + Avg_BPM2 + log_Calories + poly(Fat_Percentage, 2), data = df_female)
#summary(mod_female_v3)


# Для чоловіків
# Графік залишків
plot(mod_male, which = 1)
plot(mod_male_v4, which = 1)


# Часткові регресії
termplot(mod_male, partial.resid = TRUE)


# Для жінок
# Графік залишків
plot(mod_female, which = 1)
plot(mod_female_v2, which = 1)

# Часткові регресії
termplot(mod_female, partial.resid = TRUE)


# task 2
# mod_male_v4
# mod_female_v2

# Для чоловіків
plot(mod_male_v4, 2) 

shapiro.test(mod_male_v4$residuals) 

nortest::lillie.test(mod_male_v4$residuals) 

# Для жінок
plot(mod_female_v2, 2) 

shapiro.test(mod_female_v2$residuals) 

nortest::lillie.test(mod_female_v2$residuals) 


# Box-Cox for BMI (Y)
library(car)
bc_bmi <- powerTransform(lm(BMI ~ Age + Weight..kg. + Height_inv2 + log_Calories + Fat_Percentage2, data = df_male), family = "bcPower")
summary(bc_bmi)

# Transform the target variable
df_male$BMI_bc <- bcPower(df_male$BMI, lambda = bc_bmi$lambda)

# New model with transformed BMI
mod_male_bc <- lm(BMI_bc ~ Age + Weight..kg. + Height_inv2 + log_Calories + Fat_Percentage2, data = df_male)

# Compare normality
par(mfrow = c(1, 2))
plot(mod_male_v4, 2, main = "Original BMI")      # Original
plot(mod_male_bc, 2, main = "Transformed BMI (Box-Cox)")      # Transformed Y

# 1. Побудувати трансформацію для цільової змінної BMI
yj_bmi <- powerTransform(lm(BMI ~ Age + Weight..kg. + Height_inv2 + log_Calories + Fat_Percentage2,
                            data = df_male),
                         family = "yjPower")
lambda_yj <- yj_bmi$lambda

# 2. Застосувати трансформацію до BMI
df_male$BMI_yj <- yjPower(df_male$BMI, lambda = lambda_yj)

# 3. Побудувати нову модель
mod_male_yj <- lm(BMI_yj ~ Age + Weight..kg. + Height_inv2 + log_Calories + Fat_Percentage2, data = df_male)

# 4. Порівняти QQ-графіки
par(mfrow = c(1, 2))
plot(mod_male_v4, 2, main = "Original BMI")
plot(mod_male_yj, 2, main = "Transformed BMI (Yeo-Johnson)")

par(mfrow = c(1, 3))
hist(resid(mod_male_v4), main = "Residuals (Original)", col = "skyblue", breaks = 30)
hist(resid(mod_male_yj), main = "Residuals (Yeo-Johnson)", col = "lightgreen", breaks = 30)
hist(resid(mod_male_bc), main = "Residuals (Box-Cox)", col = "red", breaks = 30)

library(car)

# WEIGHT (Yeo-Johnson)
YJ_weight <- powerTransform(lm(Weight..kg. ~ 1, data = df_male), family = "yjPower")
lambda_weight <- YJ_weight$lambda
weight_transf <- yjPower(df_male$Weight..kg., lambda_weight)

mod_weight_transf <- lm(BMI ~ weight_transf + Age + Height_inv2 + log_Calories + Fat_Percentage2, data = df_male)

# HEIGHT (Yeo-Johnson)
YJ_height <- powerTransform(lm(Height_inv2 ~ 1, data = df_male), family = "yjPower")
lambda_height <- YJ_height$lambda
height_transf <- yjPower(df_male$Height_inv2, lambda_height)

mod_height_transf <- lm(BMI ~ weight_transf + Age + height_transf + log_Calories + Fat_Percentage2, data = df_male)

# AGE (Yeo-Johnson)
YJ_age <- powerTransform(lm(Age ~ 1, data = df_male), family = "yjPower")
lambda_age <- YJ_age$lambda
age_transf <- yjPower(df_male$Age, lambda_age)

mod_age_transf <- lm(BMI ~ Weight..kg. + age_transf + Height_inv2 + log_Calories + Fat_Percentage2, data = df_male)

# CALORIES (Yeo-Johnson)
YJ_calories <- powerTransform(lm(Calories_Burned ~ 1, data = df_male), family = "yjPower")
lambda_calories <- YJ_calories$lambda
calories_transf <- yjPower(df_male$Calories_Burned, lambda_calories)

mod_calories_transf <- lm(BMI ~ Weight..kg. + Age + Height_inv2 + calories_transf + Fat_Percentage2, data = df_male)

# For females — Avg_BPM2 (Yeo-Johnson)
YJ_avg_bpm <- powerTransform(lm(Avg_BPM ~ 1, data = df_female), family = "yjPower")
lambda_avg_bpm <- YJ_avg_bpm$lambda
avg_bpm_transf <- yjPower(df_female$Avg_BPM, lambda_avg_bpm)

mod_avg_bpm_transf <- lm(BMI ~ Age + Weight..kg. + Height_inv2 + avg_bpm_transf + log_Calories + Fat_Percentage2, data = df_female)

# Comparison plots
par(mfrow = c(1, 2))
plot(mod_male_v4, 2, main = "Original BMI")
plot(mod_weight_transf, 2, main = "Weight_transf (YJ)")

par(mfrow = c(1, 2))
plot(mod_male_v4, 2, main = "Original BMI")
plot(mod_height_transf, 2, main = "Height_transf (YJ)")

par(mfrow = c(1, 2))
plot(mod_male_v4, 2, main = "Original BMI")
plot(mod_age_transf, 2, main = "Age_transf (YJ)")

par(mfrow = c(1, 2))
plot(mod_male_v4, 2, main = "Original BMI")
plot(mod_calories_transf, 2, main = "Calories_transf (YJ)")

par(mfrow = c(1, 2))
plot(mod_female_v2, 2, main = "Original BMI (Female)")
plot(mod_avg_bpm_transf, 2, main = "Avg_BPM_transf (YJ)")




#____________________________________________________________________________
# Load required package
library(car)

# 1. Select numeric predictors
numeric_vars <- df_male[, sapply(df_male, is.numeric)]
predictor_names <- setdiff(names(numeric_vars), "BMI")  # exclude Y

# 2. Apply Yeo-Johnson transform to each X
for (var in predictor_names) {
  model_temp <- lm(as.formula(paste0(var, " ~ 1")), data = df_male)
  yj <- powerTransform(model_temp, family = "yjPower")
  lambda <- yj$lambda
  
  # Transform the variable
  transformed <- yjPower(df_male[[var]], lambda)
  
  # Store as new column
  new_name <- paste0(var, "_yj")
  df_male[[new_name]] <- transformed
}

# 3. Apply Box-Cox to BMI (or Yeo-Johnson if BMI has 0/negative values)
mod_yj_response <- powerTransform(lm(BMI ~ 1, data = df_male), family = "yjPower")
lambda_bmi <- mod_yj_response$lambda
df_male$BMI_yj <- yjPower(df_male$BMI, lambda_bmi)

# 4. Build model using transformed Y and X
# Example using some transformed predictors:
#mod_transf <- lm(BMI_yj ~ Age_yj + Weight..kg._yj + Height_inv2_yj + log_Calories_yj + Fat_Percentage2_yj, data = df_male)
mod_transf <- lm(BMI_yj ~ Age + Weight..kg. + Height_inv2 + log_Calories + Fat_Percentage2, data = df_male)
# 5. Plot comparison
par(mfrow = c(1, 2))
plot(mod_male_v4, 2, main = "Original BMI model")
plot(mod_transf, 2, main = "Transformed Y (Yeo-Johnson)")


# Для чоловіків
plot(mod_transf, 2, main = "Y transform") 

shapiro.test(mod_transf$residuals) 

nortest::lillie.test(mod_transf$residuals) 


plot(mod_male, 2, main = "original Y") 

shapiro.test(mod_male$residuals) 

nortest::lillie.test(mod_male$residuals) 



# Для жінок

# Box-Cox трансформація для BMI у жінок
bc_bmi_female <- powerTransform(lm(BMI ~ Age + Weight..kg. + Height_inv2 + Avg_BPM2 + log_Calories + Fat_Percentage2, data = df_female), family = "bcPower")
summary(bc_bmi_female)

# Трансформуємо BMI
df_female$BMI_bc <- bcPower(df_female$BMI, lambda = bc_bmi_female$lambda)

# Нова модель з трансформованим BMI
mod_female_bc <- lm(BMI_bc ~ Age + Weight..kg. + Height_inv2 + log_Calories + Fat_Percentage2, data = df_female)

# Порівняння Q-Q графіків
par(mfrow = c(1, 2))
plot(mod_female_v2, 2, main = "Original BMI")
plot(mod_female_bc, 2, main = "Transformed BMI (Box-Cox)")

shapiro.test(mod_female_v2$residuals) 

nortest::lillie.test(mod_female_v2$residuals) 


