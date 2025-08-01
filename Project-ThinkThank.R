# Load necessary libraries
library(tidyverse) # For data manipulation and visualization
library(car)       # For VIF calculation
library(rpart)     # For regression tree
library(rpart.plot) # For visualizing regression tree
library(tree)
library(readxl)
library(e1071) # for skewness

bike_data <- read_excel("/Users/thanvilalu/Downloads/806_project/2011_cycle data.xlsx")
# Display the structure and summary of the dataset
str(bike_data)
bike_data$season <- as.factor(bike_data$season)
bike_data$weathersit <- as.factor(bike_data$weathersit)
# Check for missing values and handle them if needed
sum(is.na(bike_data))
summary(bike_data)

#Check for correlation
cor(bike_data[, c("cnt", "registered", "casual", "temp","atemp", "hum", "windspeed",
                  "holiday", "workingday","weekday")]) 
# Histogram with density curve
par(mfrow=c(1,2))
rental_counts <-  bike_data$cnt
hist(rental_counts, breaks = 30, prob = TRUE, 
     main = "Histogram with Density Curve", xlab = "Rental Counts")
lines(density(rental_counts), col = "blue", lwd = 2)
  
#Taking sqrt as it is better
bike_data$sqrtcnt <- sqrt(bike_data$cnt) 
lines(density(rental_counts), col = "blue", lwd = 2)
bike_data$sqrtreg <- sqrt(bike_data$registered)
bike_data$sqrtcas <- sqrt(bike_data$casual)

################################################################################

# Combined model for sqrtcnt-overall
lm_cnt <- lm(sqrtcnt ~ temp + hum + windspeed + season + holiday + workingday + weathersit + weekday, data = bike_data) 
# Calculate VIF for predictors
vif(lm_cnt)
# Stepwise selection based on BIC
f_cnt <- ~ temp + hum + windspeed + season + holiday + workingday + weathersit + weekday
m0_cnt <- lm(sqrtcnt ~ 1, data = bike_data)
stepwise_model_cnt <- step(m0_cnt, scope = f_cnt, direction ="forward", 
                           k=log(nrow(bike_data)),trace=FALSE) 
#to understand the impact of weather and season on bike rentals, removing predictors with p > 0.05
final_model_cnt <- lm(sqrtcnt ~ temp + hum + windspeed +season+holiday  , data = bike_data) 
summary(final_model_cnt)

# Residual analysis
par(mfrow=c(2,2))
plot(fitted(final_model_cnt),residuals(final_model_cnt),xlab='Fitted',ylab='Residuals')#fitted Vs Residuals
qqnorm(residuals(final_model_cnt),ylab='Residuals');qqline(residuals(final_model_cnt))
qqPlot(residuals(final_model_cnt))#Distribution of residuals
hist(final_model_cnt$residuals) 
skewness(residuals(final_model_cnt))
#not performing shapiro as sample size is greater than 5000 

#influential points identification 
influenceIndexPlot(final_model_cnt)
cook <- cooks.distance(final_model_cnt)
cook[cook>0.5]    # check Di>0.5 

# Identify outliers
jack<-rstudent(final_model_cnt)
n<-length(jack)
pprime<-8
yn.outlier<-abs(jack)>abs(qt(0.05/(n*2),df=n-pprime-1,lower.tail=TRUE))
which(as.numeric(yn.outlier)==1)  

################################################################################

# Combined model for sqrtreg
lm_reg <- lm(sqrtreg ~ temp + hum + windspeed +
               season + holiday + workingday + weathersit + weekday, data = bike_data)
# Calculate VIF for predictors
vif(lm_reg)

# Stepwise selection based on BIC
f_reg <- ~ temp + hum + windspeed + season + holiday + workingday + weathersit + weekday
m0_reg <- lm(sqrtreg ~ 1, data = bike_data)
stepwise_model_reg <- step(m0_reg, scope = f_reg, direction ="forward", 
                           k=log(nrow(bike_data)),trace=FALSE)
#to understand the impact of weather and season on bike rentals, removing predictors with p > 0.05
final_model_reg <- lm(sqrtreg ~ temp + hum + windspeed +season + workingday+holiday , data = bike_data)
summary(final_model_reg) 

# Residual analysis
par(mfrow=c(2,2))
plot(fitted(final_model_reg),residuals(final_model_reg),xlab='Fitted',ylab='Residuals')#fitted Vs Residuals
qqnorm(residuals(final_model_reg),ylab='Residuals');qqline(residuals(final_model_reg))
qqPlot(residuals(final_model_reg))#Distribution of residuals
hist(final_model_reg$residuals) 
skewness(residuals(final_model_reg))
#not performing shapiro as sample size is greater than 5000 

#influential points identification 
influenceIndexPlot(final_model_reg)
cook <- cooks.distance(final_model_reg)
cook[cook>0.5]    # check Di>0.5 

# Identify outliers
jack<-rstudent(final_model_reg)
n<-length(jack)
pprime<-9
yn.outlier<-abs(jack)>abs(qt(0.05/(n*2),df=n-pprime-1,lower.tail=TRUE))
which(as.numeric(yn.outlier)==1)  

################################################################################

# Combined model for sqrtcas
lm_cas <- lm(sqrtcas ~ temp + hum + windspeed + season + holiday + workingday + weathersit + weekday, data = bike_data)

# Calculate VIF for predictors
vif(lm_cas)

# Stepwise selection based on BIC
f_cas <- ~   temp + hum + windspeed + season + holiday + workingday + weathersit + weekday
m0_cas <- lm(sqrtcas ~ 1, data = bike_data)
stepwise_model_cas <- step(m0_cas, scope = f_cas, direction ="forward",  k=log(nrow(bike_data)),trace=FALSE) 
#to understand the impact of weather and season on bike rentals, removing predictors with p > 0.05
final_model_cas <- lm(sqrtcas ~ temp + hum  + season + holiday + workingday , data = bike_data)
summary(final_model_cas) 

# Residual analysis
par(mfrow=c(2,2))
plot(fitted(final_model_cas),residuals(final_model_cas),xlab='Fitted',ylab='Residuals')#fitted Vs Residuals
qqnorm(residuals(final_model_cas),ylab='Residuals');qqline(residuals(final_model_cas))
qqPlot(residuals(final_model_cas))#Distribution of residuals
hist(final_model_cas$residuals) 
skewness(residuals(final_model_cas))
#not performing shapiro as sample size is greater than 5000 

#influential points identification 
influenceIndexPlot(final_model_cas)
cook <- cooks.distance(final_model_cas)
cook[cook>0.5]    # check Di>0.5 

# Identify outliers
jack<-rstudent(final_model_cas)
n<-length(jack)
pprime<-8
yn.outlier<-abs(jack)>abs(qt(0.05/(n*2),df=n-pprime-1,lower.tail=TRUE))
which(as.numeric(yn.outlier)==1)  

################################################################################
# Build regression tree
par(mfrow=c(1,1)) 
tree_cnt <- tree(sqrtcnt ~ temp + hum + windspeed + season + holiday +
                   workingday + weathersit + weekday, data = bike_data)
pruned_tree_cnt <- prune.tree(tree_cnt, best = cv.tree(tree_cnt)$size[which.min(cv.tree(tree_cnt)$dev)])
plot(pruned_tree_cnt)
title("Tree for OVerall Users")
text(pruned_tree_cnt, pretty = 0, cex = 0.8, col = "blue", font = 2)


par(mfrow=c(1,2)) 
tree_reg <- tree(sqrtreg ~ temp + hum + windspeed + season + holiday +
                   workingday + weathersit + weekday, data = bike_data)
pruned_tree_reg <- prune.tree(tree_reg, best = cv.tree(tree_reg)$size[which.min(cv.tree(tree_reg)$dev)])
plot(pruned_tree_reg)
title("Tree for Registered Users")
text(pruned_tree_reg, pretty = 0, cex = 0.8, col = "darkgreen", font = 2)

tree_cas <- tree(sqrtcas ~ temp + hum + windspeed + season + holiday +
                   workingday + weathersit + weekday, data = bike_data)
pruned_tree_cas <- prune.tree(tree_cas, best = cv.tree(tree_cas)$size[which.min(cv.tree(tree_cas)$dev)])
plot(pruned_tree_cas)
title("Tree for Casual Users")
text(pruned_tree_cas, pretty = 0, cex = 0.8, col = "red", font = 2)
 
summary(pruned_tree_cnt)
summary(pruned_tree_reg)
summary(pruned_tree_cas)

#comparing model for overall
pred_lm <- predict(final_model_cnt, newdata = bike_data) 
pred_tree <- predict(pruned_tree_cnt, newdata = bike_data)
# 2.Calculate RMSE for both models
rmse_lm <- sqrt(mean((bike_data$sqrtcnt - pred_lm)^2))
rmse_tree <- sqrt(mean((bike_data$sqrtcnt - pred_tree)^2))
# 3. Calculate MSE for both models
mse_lm <- mean((bike_data$sqrtcnt - pred_lm)^2)
mse_tree <- mean((bike_data$sqrtcnt - pred_tree)^2)
# 4. Calculate R-squared for both models
rsq_lm <- 1 - sum((bike_data$sqrtcnt - pred_lm)^2) / sum((bike_data$sqrtcnt - mean(bike_data$sqrtcnt))^2)
rsq_tree <- 1 - sum((bike_data$sqrtcnt - pred_tree)^2) / sum((bike_data$sqrtcnt - mean(bike_data$sqrtcnt))^2)
# 5. Print Results to Compare Models
cat("Stepwise Linear Model: \n");cat("RMSE:", rmse_lm, "\n");cat("MSE:", mse_lm, "\n");cat("R-squared:", rsq_lm, "\n\n")
cat("Pruned Regression Tree Model: \n");cat("RMSE:", rmse_tree, "\n");cat("MSE:", mse_tree, "\n");cat("R-squared:", rsq_tree, "\n")

#Comparing model for Registered USers 
pred_lmR <- predict(final_model_reg, newdata = bike_data) 
pred_treeR <- predict(pruned_tree_reg, newdata = bike_data)
#2. Calculate RMSE for both models
rmse_lmR <- sqrt(mean((bike_data$sqrtreg - pred_lmR)^2))
rmse_treeR <- sqrt(mean((bike_data$sqrtreg - pred_treeR)^2))
# 3. Calculate MSE for both models
mse_lmR <- mean((bike_data$sqrtreg - pred_lmR)^2)
mse_treeR <- mean((bike_data$sqrtreg - pred_treeR)^2)
# 4. Calculate R-squared for both models
rsq_lmR <- 1 - sum((bike_data$sqrtreg - pred_lmR)^2) / sum((bike_data$sqrtreg - mean(bike_data$sqrtreg))^2)
rsq_treeR <- 1 - sum((bike_data$sqrtreg - pred_treeR)^2) / sum((bike_data$sqrtreg - mean(bike_data$sqrtreg))^2)
# 5. Print Results to Compare Models
cat("Stepwise Linear Model: \n");cat("RMSE:", rmse_lmR, "\n");cat("MSE:", mse_lmR, "\n");cat("R-squared:", rsq_lmR, "\n\n")
cat("Pruned Regression Tree Model: \n");cat("RMSE:", rmse_treeR, "\n");cat("MSE:", mse_treeR, "\n");cat("R-squared:", rsq_treeR, "\n")
 

#Comparing model for Casual USers 
pred_lm1 <- predict(final_model_cas, newdata = bike_data) 
pred_tree1 <- predict(pruned_tree_cas, newdata = bike_data)
# 2.Calculate RMSE for both models
rmse_lm1 <- sqrt(mean((bike_data$sqrtcas - pred_lm1)^2))
rmse_tree1 <- sqrt(mean((bike_data$sqrtcas - pred_tree1)^2))
# 3. Calculate MSE for both models
mse_lm1 <- mean((bike_data$sqrtcas - pred_lm1)^2)
mse_tree1 <- mean((bike_data$sqrtcas - pred_tree1)^2)
# 4. Calculate R-squared for both models
rsq_lm1 <- 1 - sum((bike_data$sqrtcas - pred_lm1)^2) / sum((bike_data$sqrtcas - mean(bike_data$sqrtcas))^2)
rsq_tree1 <- 1 - sum((bike_data$sqrtcas - pred_tree1)^2) / sum((bike_data$sqrtcas - mean(bike_data$sqrtcas))^2)
# 5. Print Results to Compare Models
cat("Stepwise Linear Model: \n");cat("RMSE:", rmse_lm1, "\n");cat("MSE:", mse_lm1, "\n");cat("R-squared:", rsq_lm1, "\n\n")
cat("Pruned Regression Tree Model: \n");cat("RMSE:", rmse_tree1, "\n");cat("MSE:", mse_tree1, "\n");cat("R-squared:", rsq_tree1, "\n")


