install.packages("grf") # this packages may contain the estimators I want
#install.packages('ranger')
library(dplyr)
library(grf)

auto.mpg = read.csv("~/MachineLearning/Data/TheoryProject/auto-mpg.csv")

# split the data into training and testing. May or may not need.
smp_size <- floor(0.8 * nrow(auto.mpg))
set.seed(123)
train_ind <- sample(seq_len(nrow(auto.mpg)), size = smp_size)
train <- auto.mpg[train_ind, ]
test <- auto.mpg[-train_ind, ]

X_train = train %>% select(cylinders, displacement, horsepower, weight, acceleration, model.year, origin)
X_test = test %>% select(cylinders, displacement, horsepower, weight, acceleration, model.year, origin)
y_train = train %>% select(mpg)
y_test = test %>% select(mpg)
# train a random forest regression
r.forest = regression_forest(X_train, y_train)
