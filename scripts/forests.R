all_data <- get_data()

X_train <- all_data[[1]]
y_train <- all_data[[2]]
X_test <- all_data[[3]]
y_test <- all_data[[4]]
data <- all_data[[5]]

xgb <- xgboost(data = data.matrix(X_train),
               label =,
               nrounds = 100,
               eval_metric = 'error',
               objective = 'binary:logistic',
               num_class=2)

data[,"TARGET"] <- as.factor(data[,"TARGET"])
task <- makeClassifTask(id='basic', data=data, target="TARGET")

rf <- makeLearner('classif.ranger', predict.type = 'prob')
gbm <- makeLearner('classif.gbm', predict.type = 'prob')
xgboost <- makeLearner('classif.xgboost', predict.type = 'prob', 
                       par.vals = list(objective = "binary:logistic", eval_metric = "error", nrounds=100))

rdesc <- makeResampleDesc("CV", iters=5)

benchmark(learners = list(rf, gbm, xgboost), tasks = task, resamplings = rdesc, measures = auc)
