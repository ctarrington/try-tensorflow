library(tfestimators)
library(tidyverse)



response <- function() {
  'Species'
}

features <- function() {
  setdiff(names(iris), response())
}

# split into train, test datasets
partitions <- modelr::resample_partition(iris, c(test = 0.2, train = 0.80))
iris_subset <- as.data.frame(partitions$train)
iris_subset <- do.call('rbind', replicate(50, iris_subset, simplify = FALSE))
iris_train <- iris_subset[sample(nrow(iris_subset)), ]
iris_test  <- as.data.frame(partitions$test)

summary(iris_train)
summary(iris_test)

# construct feature columns
feature_columns <- feature_columns(
  column_numeric(features())
)

# construct classifier
classifier <- dnn_classifier(
  feature_columns = feature_columns,
  hidden_units = c(10, 10, 10),
  n_classes = 3
)

# construct input function 
iris_input_fn <- function(data) {
  input_fn(data, features = features(), response = response())
}

# train classifier with training dataset
train_results <- train(classifier, input_fn = iris_input_fn(iris_train))

# valuate with test dataset
predictions <- predict(classifier, input_fn = iris_input_fn(iris_test))
evaluation <- evaluate(classifier, input_fn = iris_input_fn(iris_test))

together <- bind_cols(iris_test, predictions)