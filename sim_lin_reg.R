# Before starting this activity please remember to clear your environment, you can do that by running the following code chunk



rm(list = ls(all=TRUE))



# Reading & Understanding the Data

#Make sure the dataset is located in your current working directory, else you can change your working directory using the "setwd()" function.



cars_data <- read.csv("cars.csv")



#Use the str() function to get a feel for the dataset. This function allows you to get the dimensions of the dataset and also helps you glance through the names, types and a few observations of variables from the dataset.



str(cars_data)





#To understand how the variables in the dataset are distributed, use the summary() function to get descriptive statistics of all the numerical variables in the dataset, for categorical variables it gives the frequencies of the different levels.



summary(cars_data)



# Exploratory analysis



#The Scatter plots below are constructed using the "plot()" function in base R. We can now visually understand the relationships between the explanatory and the response variables.



par(mfrow = c(2,2)) # Splits the plotting pane 2*2

plot(cars_data$Weight, cars_data$Price, xlab = "Weight", ylab = "Price", main = "Weight vs Price")

plot(cars_data$Mileage, cars_data$Price, xlab = "Mileage", ylab = "Price", main = "Mileage vs Price")

plot(cars_data$Disp., cars_data$Price, xlab = "Displacement", ylab = "Price", main = "Displacement vs Price")

plot(cars_data$HP, cars_data$Price, xlab = "Horse Power", ylab = "Price", main = "Horse Power vs Price")



#From the above plots, we can immediately see a few intereseting relationships. The "Weight", "HP" and "Disp." have a positive relationship with the "Price" of the car. We can also see an interesting pattern here that the "Mileage" of the car is negatively related to the "Price" of the car.

### Correlation Plot

#Now, let's plot a Correlation plot by using the "corrplot()" function from the "corrplot" package.


library(corrplot) # load the corrplot library

# the "corrplot()" function takes a correlation matrix as an input, hence we use the "cor()" function from base R to get the correlation matrix. The (use = "complete.obs") parameter tells the "cor()" function to compute correleations where only all the observations are present i.e. remove all the records with missing values and compute the correlation matrix.

cor_matrix <- cor(cars_data[,c("Weight", "Mileage", "Disp.", "HP", "Price")], use = "complete.obs")

corrplot(cor_matrix, method = "number")





# Convert Reliability to a factor

cars_data[, "Reliability"] <- as.factor(cars_data[, "Reliability"])



#It's important to check for missing values in a given dataset, handle missing values in the dataset, imputing these values is one way, the other being omitting the missing values. Here we remove the missing values.



#find out the number of missing values in the dataset

sum(is.na(cars_data))

# Remove all records with missing values

cars_data <- na.omit(cars_data)





# Subset the required columns and then use the "scale()" function for standardization

cars_numeric_data <- scale(cars_data[, !(names(cars_data) %in% c("Price", "Reliability", "Country", "Type"))])

# Replace the above variables by the standardized ones in the "cars_data" dataframe

cars_data[, !(names(cars_data) %in% c("Price", "Reliability", "Country", "Type"))] <- cars_numeric_data

str(cars_data)





#The data now has to be split into train and test sets. The test set should only be used for reporting the chosen performance metric and should not be used during the analysis. Here we use 75% of the data points to train our models.



# We can control the randomness of the sampling for future reproducibility by using the "set.seed()" function

set.seed(007)

# The "sample()" function helps us randomly sample the row numbers. The x parameter takes a vector as an input (here the vector is a sequence of numbers from 1 to the number of rows in dataset). The size parameter takes the number of elements to be randomly sampled

train_rows <- sample(x = 1:nrow(cars_data), size = 0.75*nrow(cars_data))

# We use the above indices to subset the train and test sets from the data

train_data <- cars_data[train_rows, ]

test_data <- cars_data[-train_rows, ]



# Model the Data

#Today, we'll be using simple linear regression to model the prices of cars.

## Model 1 (Weight vs Price)

#In the first model, we will use "Weight" as the explanatory variable, as it has the highest correlation with "Price", which is our response variable.



# the "lm()" function helps us build our linear regression model

model1 <- lm(formula = Price ~ Weight, data = train_data)



#calling the "summary()" function on the model we built, we get the important model details, such as the R2 value, coefficients and their significance and also the distribution of the residuals.



summary(model1)



#**Residual Plots for Model 1**

#By calling the "plot()" function on the linear regression model object, we get the residual plots. 



par(mfrow = c(2,2)) # Splits the plotting pane 2*2

plot(model1) # Plot the residual plots



## Other Models

#Below, we build multiple simple linear regression models using other variables from the dataset.

### Model 2 (Mileage vs Price)

#Get ready to build your second model! Use "lm()" to build your model and "summary()" to understand the model.



model2 <- lm(Price ~ Mileage,
data = train_data)

summary(model2)




#**Residual Plots for Model 2**



par(mfrow = c(2,2)) # Splits the plotting pane 2*2

plot(model2) # Plot the residual plots





### Model 3 (Displacement vs Price)

#Get ready to build your third model! Use "lm()" to build your model and "summary()" to understand the model.



model3 <- lm(Price ~ Disp.,
data = train_data)

summary(model3)




#**Residual Plots for Model 3**



par(mfrow = c(2,2)) # Splits the plotting pane 2*2

plot(model3) # Plot the residual plots




### Model 4 (Horse Power vs Price)

#Get ready to build your fourth model! Use "lm()" to build your model and "summary()" to understand the model.



model4 <- lm(Price ~ HP,
data = train_data)

summary(model4)




#**Residual Plots for Model 4**



par(mfrow = c(2,2)) # Splits the plotting pane 2*2

plot(model4) # Plot the residual plots



# Evaluation

## R2 values

#Here, we plot the R^2 values of the models we built, using the R^2 values we are going to pick the most satisfactory model.



# Create a vector of names of the models

model_names <- c("Weight vs Price", "Mileage vs Price",
"Disp. vs Price", "HP vs Price")

# To access the r2 values we have to use "$" on the "summary()" of the model.

# Create a vector of r2 values of all the models

r2_values <- c(summary(model1)$r.squared, summary(model2)$r.squared,
summary(model3)$r.squared, summary(model4)$r.squared)

# We use the "cbind()" function to bind those two vectors.

r2 <- cbind(model_names, r2_values)

# Construct a bar plot of the r2 values of the above models

barplot(r2_values, names.arg = r2[,1], cex.names = 0.78, ylim = c(0, 0.8),
main = "A Barplot of R^2 Values of Various Models", xlab = "Models",
ylab = "Variance in Price Explained (R^2)")



#The "Weight vs Price" model has the best R^2 score. So we pick it as the final model.



## Prediction

#Predict the prices of unseen cars, using the chosen model.



# The "predict()" function helps us predict on data using a model

preds_model <- predict(model1, test_data[, !(names(test_data) %in% c("Price"))])



## Performance Metrics

#Once we choose the model we have to report performance metrics on the test data. We are going to report three error metrics for regression.

### Error Metrics for Regression


#* Evaluation of regression models can also be done by calling the "regr.eval()" function from the "DMwR" package



library(DMwR)

regr.eval(test_data$Price, preds_model)



#The formula obtained by building our chosen regression model is: 

#$$PriceofCar = 12626.3 + 3398.8\times (ScaledWeightofCar)$$



























































