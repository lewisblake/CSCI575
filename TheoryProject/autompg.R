install.packages("grf") # this packages may contain the estimators I want
#install.packages('ranger')
library(dplyr)
library(grf)

auto.mpg = read.csv("~/MachineLearning/Data/TheoryProject/auto-mpg.csv") # Windows
auto.mpg = read.csv("/Users/lewisblake/Documents/School/Mines/F19/MachineLearning/Data/TheoryProject/auto-mpg.csv")

# split the data into training and testing
smp_size <- floor(0.8 * nrow(auto.mpg))
set.seed(123)
train_ind <- sample(seq_len(nrow(auto.mpg)), size = smp_size)
train <- auto.mpg[train_ind, ]
test <- auto.mpg[-train_ind, ]

X_train = train %>% select(cylinders, displacement, horsepower, weight, acceleration, model.year, origin)
X_test = test %>% select(cylinders, displacement, horsepower, weight, acceleration, model.year, origin)
y_train = train %>% select(mpg)
y_test = test %>% select(mpg)

# tunr verything into matrix for compatability with grf functions
X_train_mat = as.matrix(sapply(X_train, as.numeric))  
y_train_mat = as.matrix(sapply(y_train, as.numeric)) 
X_test_mat = as.matrix(sapply(X_test, as.numeric)) 
y_test_mat = as.matrix(sapply(y_test, as.numeric)) 

# train a random forest regression

r.forest = regression_forest(X_train_mat, y_train_mat, num.trees = 10000)
#c.forest = custom_forest(X_train_mat, y_train_mat)
r.forest_pred = predict(r.forest, estimate.variance = TRUE)
r.forest_test = predict(r.forest, X_test_mat, estimate.variance = TRUE)
#c.forest_pred = predict(c.forest, X_test_mat)
head(r.forest_pred) # bingo bango
plot(r.forest_pred)
plot(sort(r.forest_pred$predictions))



## Plotting predictions
library(ggplot2)
# training
r.forest_pred$min = r.forest_pred$predictions - sqrt(r.forest_pred$variance.estimates)
r.forest_pred$max = r.forest_pred$predictions + sqrt(r.forest_pred$variance.estimates)
ordered.df = r.forest_pred[order(r.forest_pred$predictions), ]

ggplot(ordered.df, aes(x = as.numeric(row.names(r.forest_pred)),  y = predictions)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = max, ymin = min))

# test
r.forest_test$min = r.forest_test$predictions - sqrt(r.forest_test$variance.estimates)
r.forest_test$max = r.forest_test$predictions + sqrt(r.forest_test$variance.estimates)
r.forest_test$mpg = test$mpg
ordered.test.df = r.forest_test[order(r.forest_test$predictions), ]

ggplot(ordered.test.df, aes(x = mpg,  y = predictions)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = max, ymin = min))+
  geom_abline(intercept = 0, lty = 3)




