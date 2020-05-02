plan <- drake_plan(
  data_raw = obtain_all_data(),
  data_raw_fixed = fix_target(data_raw),
  
  save_data = write.csv(data_raw_fixed, 
                        file_out("data/data_concataned.csv"), row.names = FALSE),
  data_imp = impute_median(data_raw_fixed),
  data_ohe = one_hot_encode(data_imp),
  data_red = remove_advanced_measures(data_ohe),
  
  cv_inds = generate_cv_inds(data_red),
  cv_desc = makeResampleDesc("CV", fixed = TRUE),
  
  measures = list(auc, acc, ppv, tpr, f1),
  
  task_0 = makeClassifTask("task_0", data_red, "TARGET", blocking = cv_inds),
  
  lrn_0_ranger = makeLearner("classif.ranger", predict.type = "prob"),
  lrn_0_xgboost = makeLearner("classif.xgboost", predict.type = "prob"),
  #lrn_0_bart = makeLearner("classif.bartMachine", predict.type = "prob"),
  
  lrn_0_logreg = makeLearner("classif.logreg", predict.type = "prob"),
  lrn_0_rpart = makeLearner("classif.rpart", predict.type = "prob"),
  
  bench_0 = benchmark(list(lrn_0_ranger,
                           lrn_0_xgboost,
                           #lrn_0_bart,
                           lrn_0_rpart,
                           lrn_0_logreg), task_0, cv_desc, measures),
  
  data_gen = create_features_for_data(data_red, c("line_code", 
                                                  "line_comment", 
                                                  "line_blank", 
                                                  "line_code_and_comment", 
                                                  "unique_operators", 
                                                  "unique_operands", 
                                                  "total_operators", 
                                                  "total_operands", 
                                                  "flow_graph")),
  
  task_1 = makeClassifTask("task_1", filter_out_infinite_and_nan(data_gen), 
                           "TARGET", blocking = cv_inds),
  
  bench_1 = benchmark(list(lrn_0_rpart,
                           lrn_0_logreg), task_1, cv_desc, measures)
)