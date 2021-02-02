#install.packages("gmodels")
library(gmodels)

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

#plot histogram of target feature i.e quality
hist(whitewine$quality)

#pair plots
plot(whitewine)

#box plot for whole data
boxplot(whitewine)

#box plot for target feature i.e quality,alcohol,density and ....
boxplot(whitewine$quality)
boxplot(whitewine$alcohol)
boxplot(whitewine$density)
boxplot(whitewine$sulphates)
boxplot(whitewine$pH)
boxplot(whitewine$residual.sugar)
boxplot(whitewine$volatile.acidity)
boxplot(whitewine$chlorides)
boxplot(whitewine$citric.acid)
boxplot(whitewine$total.sulfur.dioxide)

#correlation
cor(whitewine$alcohol, whitewine$quality)
cor(whitewine$density, whitewine$quality)
cor(whitewine$sulphates, whitewine$quality)
cor(whitewine$pH, whitewine$quality)
cor(whitewine$residual.sugar, whitewine$quality)
cor(whitewine$volatile.acidity, whitewine$quality)
cor(whitewine$chlorides, whitewine$quality)
cor(whitewine$citric.acid, whitewine$quality)
cor(whitewine$total.sulfur.dioxide, whitewine$quality)

#linear model with all the features
linearModel <- lm(quality ~., data = whitewine )

#summary of the model
summary(linearModel)

#shuffle and split data
set.seed(124)
split <- sample(nrow(whitewine), size = floor(0.75 * nrow(whitewine)))
trainData <- whitewine[split, ]
testData <- whitewine[-split, ]
head(trainData)
head(testData)

#linear model with all the features
firstLinearModel <- lm(quality ~., data = trainData)

#summary of the model
summary(firstLinearModel)

#display 4 plots side by side for first Linear Model
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(firstLinearModel)

#apply first linear model to the test data
first_model_prediction <- predict(firstLinearModel, newdata = testData)

#linear model using only significant features. 
#the feature total.sulfur.dioxide is eliminated from the model
secondLinearModel <- lm(quality ~ citric.acid + chlorides + fixed.acidity + volatile.acidity + residual.sugar+ free.sulfur.dioxide + density + pH + sulphates + alcohol, data = trainData) 
summary(secondLinearModel)

#apply second linear model to the test data
second_model_prediction <- predict(secondLinearModel, newdata = testData)

#linear model using only significant features
#the features total.sulfur.dioxide and chlorides are eliminated from the model
thirdLinearModel <- lm(quality ~ citric.acid + fixed.acidity + volatile.acidity + residual.sugar+ free.sulfur.dioxide + density + pH + sulphates + alcohol, data = trainData) 
summary(thirdLinearModel)

#apply third linear model to the test data
third_model_prediction <- predict(thirdLinearModel, newdata = testData)

#linear model using only significant features
#the features total.sulfur.dioxide, citric.acid and chlorides are eliminated from the model
fourthLinearModel <- lm(quality ~ fixed.acidity + volatile.acidity + residual.sugar+ free.sulfur.dioxide + density + pH + sulphates + alcohol, data = trainData) 
summary(fourthLinearModel)

#apply fourth linear model to the test data
fourth_model_prediction <- predict(fourthLinearModel, newdata = testData)


#linear model using only significant features
#the features fixed.acidity and free.sulfur.dioxide are eliminated from the fourth model
fifthLinearModel <- lm(quality ~ volatile.acidity + residual.sugar + density + pH + sulphates + alcohol, data = trainData) 
summary(fifthLinearModel)

#apply linear model to the test data
fifth_model_prediction <- predict(fifthLinearModel, newdata = testData)

#display 4 plots side by side for fifth linear model
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(linearModel)

#display 4 plots side by side for fifth linear model
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(fifthLinearModel)

#print prediction results
head(fifth_model_prediction)
head(testData$quality)

#RMSE of models
sqrt(mean(fifthLinearModel$residuals^2))
sqrt(mean(linearModel$residuals^2))

#coefficients of models
linearModel$coefficients
fifthLinearModel$coefficients

#summary
summary(linearModel)
summary(fifthLinearModel)

#calculate prediction accuracy and error rates for the final model

#actual predictions data frame
actuals_predictions <- data.frame(cbind(actuals=testData$quality, predicteds=fifth_model_prediction))  
head(actuals_predictions)
#correlation accuracy
correlation_accuracy <- cor(actuals_predictions)

#Min-Max-Accuracy
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
#Mean Absolute Percentage Error
mape <- mean(abs((actuals_predictions$predicteds - actuals_predictions$actuals))/actuals_predictions$actuals)
min_max_accuracy # 90.81%, min_max accuracy
mape # 10.26%, mean absolute percentage deviation


#predict for a new data 
newdata = data.frame(volatile.acidity=0.6, residual.sugar=2.8, density=0.9940, pH=4.30,sulphates=0.4,alcohol=10.5) 
predict(fifthLinearModel, newdata) 
predict(fifthLinearModel, newdata, interval="confidence")
predict(fifthLinearModel, newdata, interval="predict")
