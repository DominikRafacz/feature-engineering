
get_data <- function() {
  data <- read.csv('data/JM1_renamed.csv')
  
  size <- floor(0.8 * nrow(data))
  train_indexes <- sample(seq_len(nrow(data)), size=size)
  
  train <- data[train_indexes, ]
  test <- data[-train_indexes, ]
  
  X_train <- select(train, -TARGET)
  y_train <- train[, 'TARGET']
  
  X_test <- select(test, -TARGET)
  y_test <- test[, 'TARGET']
  return(list(X_train, y_train, X_test, y_test, train, test, data))
}






