# Installation of required packages
packages <- c("MASS", "car", "carData", "readxl", "rgl", "rmarkdown", "nortest",
              "latex2exp", "pca3d", "ISLR", "pls", "corrplot", "glmnet",
              "mvtnorm", "biglm", "leaps", "lme4", "viridis", "ffbase",
              "ks", "KernSmooth", "nor1mix", "np", "locfit",
              "manipulate", "mice", "VIM", "nnet")
install.packages(packages)

##########################################################################################
#                                 Лабораторна робота 1                                   #
##########################################################################################


# Load packages
lapply(packages, library, character.only = TRUE)

data <- read.table(file = "data/gym_members_exercise_tracking.csv", header = TRUE, sep = ",")

#################################### Описовий аналіз для кожної змінної #############################
# Визначимо тип змінних (якісна/кількісна)
str(data)
summary(data)

# Для кількісних змінних
quant_vars <- sapply(data, is.numeric)

Mean = sapply(data[, quant_vars], mean, na.rm = TRUE)
Median = sapply(data[, quant_vars], median, na.rm = TRUE)
SD = sapply(data[, quant_vars], sd, na.rm = TRUE)

# Для якісних змінних
categorical_vars <- data[sapply(data, is.factor) | sapply(data, is.character)]

for (col in names(categorical_vars)) {
  cat("Частоти для змінної:", col, "\n")
  print(table(categorical_vars[[col]]))
  cat("\n")
}


#Розвідувальний аналіз

# Візуалізація: гістограми для кожної змінної
for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    hist(data[[col]], main = paste("Гістограма для", col),
         xlab = col,  
         breaks = 30)
  }
}

missing_values <- colSums(is.na(data))
missing_values


# Створення діаграми розсіювання для всіх змінних у наборі даних
scatterplotMatrix(data[sapply(data, is.numeric)], 
                  col = 1,  
                  regLine = list(col = 2), 
                  smooth = list(col.smooth = 4, col.spread = 4)) 

#Проста лінійна регресія для кожної змінної
# Вибір числових змінних для аналізу
data_numeric <- data[sapply(data, is.numeric)]

# Перевірка на кількість числових змінних
num_vars <- ncol(data_numeric)


# Застосування простого лінійного регресійного аналізу для кожної змінної
for (i in 1:ncol(data_numeric)) {
  X <- data_numeric[, i]
  
  model <- lm(data$BMI ~ X)
  
  cat("Коефіцієнти регресії для змінної", names(data_numeric)[i], ":\n")
  print(summary(model)$coefficients)
  
  cat("R² для змінної", names(data_numeric)[i], ":", summary(model)$r.squared, "\n\n")
}

######################################################################################
#  Task2 Перевірка припущень
# BMI ~ Age
bmi_age <- lm(BMI ~ Age, data = data)
sm<-summary(bmi_age)
sm

plot(data$Age, data$BMI)
abline(bmi_age, col=2)

sum(resid(bmi_age)^ 2)
var(data$Age)
var(data$BMI)
hist(data$Age)
hist(data$BMI)
plot(sm$residuals)
abline(h = 0, col = "red")
mean(sm$residuals)
var(sm$residuals)
hist(sm$residuals)

# BMI ~ Weight..kg.
bmi_weight <- lm(BMI ~ Weight..kg., data = data)
sm2<-summary(bmi_weight)
sm2

plot(data$Weight..kg., data$BMI)
abline(bmi_weight, col=2)

sum(resid(bmi_weight)^ 2)
var(data$Weight..kg.)
var(data$BMI)
hist(data$Weight..kg.)
hist(data$BMI)
plot(sm2$residuals)
abline(h = 0, col = "red")
mean(sm2$residuals)
var(sm2$residuals)
hist(sm2$residuals)

# Weight ~ Height..m.
weight_height <- lm(Weight..kg. ~ Height..m., data = data)
sm3<-summary(weight_height)
sm3

plot(data$Height..m., data$Weight..kg.)
abline(weight_height, col=2)

sum(resid(weight_height)^ 2)
var(data$Height..m.)
var(data$Weight..kg.)
hist(data$Height..m.)
hist(data$Weight..kg.)
plot(sm3$residuals)
abline(h = 0, col = "red")
mean(sm3$residuals)
var(sm3$residuals)
hist(sm3$residuals)

# Height..m. ~ Water_Intake..liters.
height_water_intake <- lm(Height..m. ~ Water_Intake..liters., data = data)
sm4<-summary(height_water_intake)
sm4

plot(data$Height..m., data$Water_Intake..liters.)
abline(height_water_intake, col=2)

sum(resid(height_water_intake)^ 2)
var(data$Water_Intake..liters.)
var(data$Height..m.)
hist(data$Water_Intake..liters.)
hist(data$Height..m.)
plot(sm4$residuals)
abline(h = 0, col = "red")
mean(sm4$residuals)
var(sm4$residuals)
hist(sm4$residuals)

# Calories_Burned ~ Session_Duration..hours.
session_dur_calories <- lm(Session_Duration..hours. ~ Calories_Burned, data = data)
sm5<-summary(session_dur_calories)
sm5

plot(data$Calories_Burned, data$Session_Duration..hours.)
abline(coef = sm5$coefficients, col="red")

sum(resid(session_dur_calories)^ 2)
var(data$Calories_Burned)
var(data$Session_Duration..hours.)
hist(data$Calories_Burned)
hist(data$Session_Duration..hours.)
plot(sm5$residuals)
abline(h = 0, col = "red")
mean(sm5$residuals)
var(sm5$residuals)
hist(sm5$residuals)

# Calories_Burned ~ Fat_Percentage
calories_fat <- lm(Calories_Burned ~ Fat_Percentage, data = data)
sm6<-summary(calories_fat)
sm6

plot(data$Fat_Percentage, data$Calories_Burned)
abline(coef = sm6$coefficients, col="red")

sum(resid(calories_fat)^ 2)
var(data$Fat_Percentage)
var(data$Calories_Burned)
hist(data$Fat_Percentage)
hist(data$Calories_Burned)
plot(sm6$residuals)
abline(h = 0, col = "red")
mean(sm6$residuals)
var(sm6$residuals)
hist(sm6$residuals)
# Fat_Percentage ~ Water_Intake..liters.
fat_water <- lm(Fat_Percentage ~ Water_Intake..liters., data = data)
sm7<-summary(fat_water)
sm7

plot(data$Water_Intake..liters., data$Fat_Percentage)
abline(coef = sm7$coefficients, col="red")

sum(resid(fat_water)^ 2)
var(data$Water_Intake..liters.)
var(data$Fat_Percentage)
hist(data$Water_Intake..liters.)
hist(data$Fat_Percentage)
plot(sm7$residuals)
abline(h = 0, col = "red")
mean(sm7$residuals)
var(sm7$residuals)
hist(sm7$residuals)

# Experience_Level ~ Workout_Frequency..days.week.
exp_freq <- lm(Experience_Level ~ Workout_Frequency..days.week., data = data)
sm8<-summary(exp_freq)
sm8

plot(data$Workout_Frequency..days.week., data$Experience_Level)
abline(coef = sm8$coefficients, col="red")

sum(resid(exp_freq)^ 2)
var(data$Workout_Frequency..days.week.)
var(data$Experience_Level)
hist(data$Workout_Frequency..days.week.)
hist(data$Experience_Level)
plot(sm8$residuals)
abline(h = 0, col = "red")
mean(sm8$residuals)
var(sm8$residuals)
hist(sm8$residuals)


##########################################################################################
#                                 Лабораторна робота 2                                   #
##########################################################################################
data$GenderBinary <- ifelse(data$Gender == "Male", 1, 0)
data$Gender <- NULL
###############################Завдання 1########################################
car::scatterplotMatrix(data[sapply(data, is.numeric)], col = 1, regLine = list(col = 2),
                       smooth = list(col.smooth = 4, col.spread = 4))


model_1 <- lm(data$BMI ~ ., data = data)
summary(model_1)

summary(model_1)$coefficients

car::vif(model_1)

###############################Завдання 2########################################

model_2 <- lm(data$BMI ~  data$Height..m. + data$Weight..kg. + data$Gender + data$Age, data = data)
summary(model_2)

model_3 <- lm(data$BMI ~  data$Height..m. + data$Weight..kg. + data$Gender, data = data)
summary(model_3)


anova(model_2, model_3)
car::compareCoefs(model_1, model_2, model_3)


###############################Завдання 3########################################

summary(model_1)$coefficients

summary(model_1)

confint(model_2, level = 0.90)
confint(model_2, level = 0.95)
confint(model_2, level = 0.99)

res_pred <- predict(model_2, newdata = data, interval = "confidence", level = 0.95)
res_pred

# Завантаження бібліотек
library(ggplot2)

# Припускаємо, що res_pred - це результати прогнозу
# Якщо res_pred - це матриця, перетворимо її у data frame
df_pred <- data.frame(
  Index = 1:nrow(res_pred),
  Fit = res_pred[, "fit"],
  Lwr = res_pred[, "lwr"],
  Upr = res_pred[, "upr"]
)

# Побудова графіка з довірчим інтервалом
ggplot(df_pred, aes(x = Index, y = Fit)) +
  geom_line(color = "blue", size = 1) +  # Лінія прогнозованих значень
  geom_ribbon(aes(ymin = Lwr, ymax = Upr), fill = "red", alpha = 1) +  # Довірчий інтервал
  labs(title = "Прогноз із 95% довірчим інтервалом",
       x = "Спостереження", y = "Прогнозоване значення") +
  theme_minimal()

res_pred_v2 <- predict(model_2, newdata = data, interval = "confidence", level = 0.99)
res_pred_v2

new_data_v1 <- data

# Визначаємо, які колонки є числовими
numeric_cols <- sapply(new_data_v1, is.numeric)

# Множимо тільки числові змінні, крім BMI
new_data_v1[, numeric_cols & names(new_data_v1) != "BMI"] <- new_data_v1[, numeric_cols & names(new_data_v1) != "BMI"] * 1.1
new_data_v1
data
predict(model_2, newdata = data, interval = "prediction", level = 0.95)

predict(model_2, newdata = new_data_v1, interval = "prediction", level = 0.95)








