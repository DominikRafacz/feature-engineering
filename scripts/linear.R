source("scripts/prepare_data.R")

all_data <- get_data()

X_train <- all_data[[1]]
y_train <- all_data[[2]]
X_test <- all_data[[3]]
y_test <- all_data[[4]]
train <- all_data[[5]]
test <- all_data[[6]]
data <- all_data[[7]]

model <- lm(TARGET~.,train)
