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
#full_model <- lm(Calories_Burned ~ ., data = data1)
full_model <- lm(BMI ~ ., data = data1)
summary(full_model)

#start_model <- lm(Calories_Burned ~ Age + Gender + Avg_BPM + Session_Duration..hours. + BMI + Experience_Level, data = data1)
#start_model <- lm(BMI ~ ., data = data1)
start_model <- lm(BMI ~ Age + Gender + Weight..kg. + Height..m. + Avg_BPM + Session_Duration..hours. + Calories_Burned, data = data1)
summary(start_model)

mod_AIC <- MASS::stepAIC(start_model, direction = "both", k = 2)
summary(mod_AIC)

mod_BIC <- MASS::stepAIC(start_model, direction = "both", k = log(nrow(data1))) 
summary(mod_BIC)

#--------------------------------------------------------------
# AIC 
AIC_mod_backward <- MASS::stepAIC(full_model, direction = "backward", k = 2)
summary(AIC_mod_backward)
AIC_mod_forward <- MASS::stepAIC(lm(BMI ~ 1, data = data1),
                                 scope = list(lower = ~ 1, upper = formula(full_model)),
                                 direction = "forward", k = 2)

AIC_mod_both <- MASS::stepAIC(full_model, direction = "both", k = 2)

# BIC

BIC_mod_backward <- MASS::stepAIC(full_model, direction = "backward", k = log(nrow(data1)))

BIC_mod_forward <- MASS::stepAIC(lm(BMI ~ 1, data = data1),
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

#----------------------task 2.3
data2 <- data
data2$Gender <- ifelse(data2$Gender == "Female", 1, 0)
data2$Workout_Type <- as.factor(data1$Workout_Type)
data2$Experience_Level <- as.factor(data1$Experience_Level)
data2$Workout_Frequency..days.week.<- as.factor(data1$Workout_Frequency..days.week.)
data2

# Group settings for plotting
col <- data2$Gender + 3                      # Different color for each gender (col=3 -> green, col=4 -> blue)
cex <- 0.5 + 0.5 * data2$Gender              # Slightly bigger points for females

# 1. No dummy variable
(mod1 <- lm(BMI ~ Weight..kg., data = data2))
plot(BMI ~ Weight..kg., data = data2, col = col, pch = 16, cex = cex, main = "1. No dummy variable")
abline(coef = mod1$coefficients, lwd = 2)

# 2. Dummy variable (Gender) included additively
(mod2 <- lm(BMI ~ Weight..kg. + Gender, data = data2))
plot(BMI ~ Weight..kg., data = data2, col = col, pch = 16, cex = cex, main = "2. Additive Gender")
abline(a = mod2$coefficients[1], b = mod2$coefficients[2], col = 3, lwd = 2)  # Male
abline(a = mod2$coefficients[1] + mod2$coefficients[3], 
       b = mod2$coefficients[2], col = 4, lwd = 2)  # Female
summary(mod2)

# 3. Gender with interaction
(mod3 <- lm(BMI ~ Weight..kg. + Gender + Weight..kg.:Gender, data = data2))
plot(BMI ~ Weight..kg., data = data2, col = col, pch = 16, cex = cex, main = "3. Gender + Interaction")
abline(a = mod3$coefficients[1], b = mod3$coefficients[2], col = 3, lwd = 2)  # Male
abline(a = mod3$coefficients[1] + mod3$coefficients[3], 
       b = mod3$coefficients[2] + mod3$coefficients[4], col = 4, lwd = 2)  # Female
summary(mod3)

# 4. Gender only in interaction
(mod4 <- lm(BMI ~ Weight..kg. + Weight..kg.:Gender, data = data2))
plot(BMI ~ Weight..kg., data = data2, col = col, pch = 16, cex = cex, main = "4. Interaction Only")
abline(a = mod4$coefficients[1], b = mod4$coefficients[2], col = 3, lwd = 2)  # Male
abline(a = mod4$coefficients[1], 
       b = mod4$coefficients[2] + mod4$coefficients[3], col = 4, lwd = 2)  # Female
summary(mod4)

# 5. Dummy only (Gender)
(mod5 <- lm(BMI ~ Gender, data = data2))
plot(BMI ~ Weight..kg., data = data2, col = col, pch = 16, cex = cex, main = "5. Gender only")
abline(a = mod5$coefficients[1], b = 0, col = 3, lwd = 2)  # Male
abline(a = mod5$coefficients[1] + mod5$coefficients[2], b = 0, col = 4, lwd = 2)  # Female
summary(mod5)

# 6. Gender in intercept and slope via interaction
(mod6 <- lm(BMI ~ Gender + Weight..kg.:Gender, data = data2))
plot(BMI ~ Weight..kg., data = data2, col = col, pch = 16, cex = cex, main = "6. Intercept & Slope")
abline(a = mod6$coefficients[1], b = 0, col = 3, lwd = 2)  # Male
abline(a = mod6$coefficients[1] + mod6$coefficients[2], 
       b = mod6$coefficients[3], col = 4, lwd = 2)  # Female
summary(mod6)

# 7. Interaction in slope only
(mod7 <- lm(BMI ~ Weight..kg.:Gender, data = data2))
plot(BMI ~ Weight..kg., data = data2, col = col, pch = 16, cex = cex, main = "7. Slope Only")
abline(a = mod7$coefficients[1], b = 0, col = 3, lwd = 2)  # Male
abline(a = mod7$coefficients[1], b = mod7$coefficients[2], col = 4, lwd = 2)  # Female
summary(mod7)
