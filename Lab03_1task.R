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

summary(data)

data1 <- data
data1$Gender <- as.factor(data1$Gender)
data1$Workout_Type <- as.factor(data1$Workout_Type)
data1$Experience_Level <- as.factor(data1$Experience_Level)
data1$Workout_Frequency..days.week.<- as.factor(data1$Workout_Frequency..days.week.)
#------------------------------------------------------------
summary(data1)
str(data1)
# перші 5
car::scatterplotMatrix(data1[, 1:5], col = 1, regLine = list(col = 2),
                       smooth = list(col.smooth = 4, col.spread = 4))
# наступні 5 
car::scatterplotMatrix(data1[, 6:10], col = 1, regLine = list(col = 2),
                       smooth = list(col.smooth = 4, col.spread = 4))
# останні 5 
car::scatterplotMatrix(data1[, 6:15], col = 1, regLine = list(col = 2),
                       smooth = list(col.smooth = 4, col.spread = 4))

#----------------------- Завдання 1 ---------------------------
full_model <- lm(Calories_Burned ~ ., data = data1)
summary(mod)

start_model <- lm(Calories_Burned ~ Age + Gender + Avg_BPM + Session_Duration..hours. + BMI + Experience_Level, data = data1)
summary(start_model)

mod_AIC <- MASS::stepAIC(start_model, direction = "both", k = 2)
summary(mod_AIC)

mod_BIC <- MASS::stepAIC(start_model, direction = "both", k = log(nrow(data1))) 
summary(mod_BIC)

#--------------------------------------------------------------
# AIC 
AIC_mod_backward <- MASS::stepAIC(full_model, direction = "backward", k = 2)
summary(AIC_mod_backward)
AIC_mod_forward <- MASS::stepAIC(lm(Calories_Burned ~ 1, data = data1),
                       scope = list(lower = ~1, upper = formula(full_model)),
                       direction = "forward", k = 2)

AIC_mod_both <- MASS::stepAIC(full_model, direction = "both", k = 2)

# BIC

BIC_mod_backward <- MASS::stepAIC(full_model, direction = "backward", k = log(nrow(data1)))

BIC_mod_forward <- MASS::stepAIC(lm(Calories_Burned ~ 1, data = data1),
                                 scope = list(lower = ~1, upper = formula(full_model)),
                                 direction = "forward", k = log(nrow(data1)))

BIC_mod_both <- MASS::stepAIC(full_model, direction = "both", k = log(nrow(data1)))

#-------------------------------------------------------------
car::compareCoefs(AIC_mod_backward, BIC_mod_backward)
car::compareCoefs(AIC_mod_forward, BIC_mod_forward)
car::compareCoefs(AIC_mod_both, BIC_mod_both)

formula(AIC_mod_backward)
formula(AIC_mod_forward)
formula(AIC_mod_both)
formula(BIC_mod_backward)
formula(BIC_mod_forward)
formula(BIC_mod_both)

#---------------------R^2
summary(AIC_mod_backward)$adj.r.squared
summary(AIC_mod_forward)$adj.r.squared
summary(AIC_mod_both)$adj.r.squared
summary(BIC_mod_backward)$adj.r.squared
summary(BIC_mod_forward)$adj.r.squared
summary(BIC_mod_both)$adj.r.squared

#---------------------AIC 
AIC(AIC_mod_backward)
AIC(AIC_mod_forward)
AIC(AIC_mod_both)
AIC(BIC_mod_backward)
AIC(BIC_mod_forward)
AIC(BIC_mod_both)

#---------------------BIC 
BIC(AIC_mod_backward)
BIC(AIC_mod_forward)
BIC(AIC_mod_both)
BIC(BIC_mod_backward)
BIC(BIC_mod_forward)
BIC(BIC_mod_both)
