make_basic_benchmark <- function(data_raw) {
  task <-
    makeClassifTask(id = 'basic',
                    data = na.omit(select(data_raw, -source)),
                    target = "TARGET")
  
  rf <- makeLearner('classif.ranger', predict.type = 'prob')
  gbm <- makeLearner('classif.gbm', predict.type = 'prob')
  xgboost <- makeLearner(
    'classif.xgboost',
    predict.type = 'prob',
    par.vals = list(
      objective = "binary:logistic",
      eval_metric = "error",
      nrounds = 100
    )
  )
  lda <- makeLearner('classif.lda', predict.type = 'prob')
  linear <- makeLearner('classif.logreg', predict.type = 'prob')
  
  rdesc <- makeResampleDesc("CV", iters = 5)
  
  benchmark(
    learners = list(lda, linear, rf, gbm, xgboost),
    tasks = task,
    resamplings = rdesc,
    measures = auc
  )
}