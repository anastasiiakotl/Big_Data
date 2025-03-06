# Installation of required packages
packages <- c("MASS", "car", "readxl", "rgl", "rmarkdown", "nortest",
              "latex2exp", "pca3d", "ISLR", "pls", "corrplot", "glmnet",
              "mvtnorm", "biglm", "leaps", "lme4", "viridis", "ffbase",
              "ks", "KernSmooth", "nor1mix", "np", "locfit",
              "manipulate", "mice", "VIM", "nnet")
install.packages(packages)

# Load packages
lapply(packages, library, character.only = TRUE)

data <- read.table(file = "gym_members_exercise_tracking.csv", header = TRUE, sep = ",")

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

