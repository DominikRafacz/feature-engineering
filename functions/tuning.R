tune_rpart <- function(task, measures, cv_desc) {
  param_set <- makeParamSet(
    makeIntegerParam("minsplit", 1, 100),
    makeIntegerParam("minbucket", 1, 100),
    makeNumericParam("cp", 0, 1),
    makeIntegerParam("maxcompete", 1, 7),
    makeIntegerParam("maxsurrogate", 0, 10),
    makeIntegerParam("xval", 1, 10),
    makeIntegerParam("maxdepth", 2, 32)
  )
  
  control <- makeMBOControl()
  control <- setMBOControlTermination(control, iters = 100)
  tune_control <- makeTuneControlMBO(mbo.control = control)
  res <- tuneParams(makeLearner("classif.rpart", predict.type = "prob"), 
                    task = task, 
                    resampling = cv_desc, 
                    par.set = param_set, 
                    control = tune_control, 
                    measures = measures,
                    show.info = FALSE)
  
  res$x
}