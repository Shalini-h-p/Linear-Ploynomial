library(ggplot2)
#read the data
wine_white <- "data/winequality-white.csv"
whitewine <- read.csv(wine_white, header = TRUE, sep = ";")

#look at the first few rows of the data, the structure 
View(whitewine)
head(whitewine)

#check for the data type of all the features
str(whitewine)

#summary of data set
summary(whitewine)

#linear model with all the features
dataModel_1 <- lm(quality ~., data = whitewine)

#summary of the model
summary(dataModel_1)

#plot the each attribute with respect to the target featute
plot(whitewine$alcohol, whitewine$quality, pch=2)
abline(lm(quality ~ alcohol, data = whitewine), col="blue")

plot(whitewine$volatile.acidity, whitewine$quality, pch=2)
abline(lm(quality ~ volatile.acidity, data = whitewine), col="blue")

plot(whitewine$density, whitewine$quality, pch=2)
abline(lm(quality ~ density, data = whitewine), col="blue")

plot(whitewine$residual.sugar, whitewine$quality, pch=2)
abline(lm(quality ~ residual.sugar, data = whitewine), col="blue")

plot(whitewine$pH, whitewine$quality, pch=2)
abline(lm(quality ~ pH, data = whitewine), col="blue")

plot(whitewine$sulphates , whitewine$quality, pch=2)
abline(lm(quality ~ sulphates , data = whitewine), col="blue")

#shuffle and split data
set.seed(123)
split <- sample(nrow(whitewine), size = floor(0.7 * nrow(whitewine)))
training <- whitewine[split, ]
validation <- whitewine[-split, ]
head(training)
head(validation)

#linear model using only significant features
dataModel_2 <- lm(quality ~ volatile.acidity + residual.sugar + density + pH + sulphates + alcohol, data = training) 

#summary of the model
summary(dataModel_2)

#polynomial regression model
dataModel_3 <- lm(quality ~ volatile.acidity + residual.sugar + pH + sulphates +  polym(alcohol,density, degree=2), data = training)
summary(dataModel_3)

#polynomial regression model
dataModel_4 <- lm(quality ~ residual.sugar + pH + sulphates +  polym(volatile.acidity,alcohol,density, degree=2), data = training)
summary(dataModel_4)

#polynomial regression model
dataModel_5 <- lm(quality ~  pH + sulphates +  polym(residual.sugar,volatile.acidity,alcohol,density, degree=2), data = training)
summary(dataModel_5)

#polynomial regression model 
dataModel_6 <- lm(quality ~   sulphates +  polym(pH,residual.sugar,volatile.acidity,alcohol,density, degree=2), data = training)
summary(dataModel_6)

#polynomial regression model with degree 2
dataModel_6 <- lm(quality ~ polym(sulphates,pH,residual.sugar,volatile.acidity,alcohol,density, degree=2), data = training)
summary(dataModel_6)

#apply linear model to the test data
prediction_model_1 <- predict(dataModel_1, newdata = validation)
prediction_model_2 <- predict(dataModel_6, newdata = validation)

#print prediction results
head(prediction_model_2)
head(validation$quality)

#RMSE of linear model
sqrt(mean(dataModel_1$residuals^2))

#RMSE of polynomial mode
sqrt(mean(dataModel_6$residuals^2))

#coefficients of models
dataModel_1$coefficients
dataModel_6$coefficients

#summary
summary(dataModel_1)
summary(dataModel_6)

#calculate prediction accuracy and error rates for the linear and polynomial models
#1. Linear Model
#actual predictions dataframe
actuals_predictions_linear <- data.frame(cbind(actuals=validation$quality, predicteds=prediction_model_1))
head(actuals_predictions_linear)
#correlation accuracy
correlation_accuracy <- cor(actuals_predictions_linear)

#Min-Max-Accuracy
min_max_accuracy <- mean(apply(actuals_predictions_linear, 1, min) / apply(actuals_predictions_linear, 1, max))
#Mean Absolute Percentage Error
mape <- mean(abs((actuals_predictions_linear$predicteds - actuals_predictions_linear$actuals))/actuals_predictions_linear$actuals)
min_max_accuracy # 90.86%, min_max accuracy
mape # 10.19%, mean absolute percentage deviation

#2. Polynomial model

#actual predictions dataframe
actuals_predictions_poly <- data.frame(cbind(actuals=validation$quality, predicteds=prediction_model_2))
head(actuals_predictions_poly)
#correlation accuracy
correlation_accuracy <- cor(actuals_predictions_poly)

#Min-Max-Accuracy
min_max_accuracy <- mean(apply(actuals_predictions_poly, 1, min) / apply(actuals_predictions_poly, 1, max))
#Mean Absolute Percentage Error
mape <- mean(abs((actuals_predictions_poly$predicteds - actuals_predictions_poly$actuals))/actuals_predictions_poly$actuals)
min_max_accuracy # 88.30%, min_max accuracy
mape # 13.91%, mean absolute percentage deviation


#display 4 plots side by side for first model
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(dataModel_1)

#display 4 plots side by side for final model
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(dataModel_6)

#two-way ANOVA for all the models
anova_1 <- aov(dataModel_1)
anova_2 <- aov(dataModel_2)
anova_3 <- aov(dataModel_3)
anova_4 <- aov(dataModel_4)
anova_5 <- aov(dataModel_5)
anova_6 <- aov(dataModel_6)
anova_1 #Residual standard error: 0.7513569
anova_2 #Residual standard error: 0.7592633
anova_3 #Residual standard error: 0.7558109
anova_4 #Residual standard error: 0.7533226
anova_5 #Residual standard error: 0.751193
anova_6 #Residual standard error: 0.7442314
