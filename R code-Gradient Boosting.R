# ML2 Gradient Boosting (GB): using xgboost, caret package to train on the previous dataset with PCA
library(caret)
library(lattice)
library(xgboost)

set.seed(123)

#
# 1. Gradient Boosting on dataset with one pricipal component
# Fit the model on the training set
modelxg1 <- train(
  label ~., data = train1, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)

# Best tuning parameter
modelxg1$bestTune

# Make predictions on the test data
predicted.classes <- modelxg1 %>% predict(test1)
head(predicted.classes)

# Compute model prediction accuracy rate
mean(predicted.classes == test1$label)

#
# 2. Gradient Boosting on dataset with 75 pricipal components
# Fit the model on the training set
modelxg75 <- train(
  label ~., data = train75, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)

# Best tuning parameter
modelxg75$bestTune

# Make predictions on the test data
predicted.classes75 <- modelxg75 %>% predict(test75)
head(predicted.classes75)

# Compute model prediction accuracy rate
mean(predicted.classes75 == test75$label)

#
# 3. Gradient Boosting on dataset with 150 pricipal components
# Fit the model on the training set
modelxg150 <- train(
  label ~., data = train150, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)

# Best tuning parameter
modelxg150$bestTune

# Make predictions on the test data
predicted.classes150 <- modelxg150 %>% predict(test150)
head(predicted.classes150)

# Compute model prediction accuracy rate
mean(predicted.classes150 == test150$label)



