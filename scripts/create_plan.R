plan <- drake_plan(
  # obtaining data
  data_raw = obtain_all_data(),
  
  # basic transformations
  data_raw_fixed = fix_target(data_raw),
  data_one_hot_encoded = one_hot_encode(data_raw_fixed),
  data_imputed = impute_median(data_one_hot_encoded),
  data_reduced = remove_advanced_measures(data_imputed),
  data_basic_Z = data_reduced,
  data_basic_N = filter(data_reduced, line_code != 0),
  
  
  # saving data
  save_data = write.csv(data_reduced, 
                        file_out("data/data_concataned.csv"), row.names = FALSE),
  
  
  # generating indices
  cv_inds_Z = generate_cv_inds(data_basic_Z),
  cv_inds_N = generate_cv_inds(data_basic_N),
  
  # CV description
  cv_desc = makeResampleDesc("CV", fixed = TRUE, iters = 5),
  
  # measures
  measures = list(auc, acc, ppv, tpr, f1),
  
  # models
  lrn_ranger = makeLearner("classif.ranger", id = "ranger", predict.type = "prob"),
  lrn_logreg = makeLearner("classif.logreg", id = "logreg", predict.type = "prob"),
  lrn_rpart  = makeLearner("classif.rpart",  id = "rpart",  predict.type = "prob"),
  lrn_knn    = makeLearner("classif.kknn",   id = "kknn",   predict.type = "prob"),
  lrns_wb = list(lrn_logreg,
                 lrn_rpart,
                 lrn_knn),
  
  # null task
  task_basic_Z = makeClassifTask("task_null_Z", data_basic_Z, "TARGET", blocking = cv_inds_Z),
  task_basic_N = makeClassifTask("task_null_N", data_basic_N, "TARGET", blocking = cv_inds_N),
  bench_basic = benchmark(c(lrns_wb, list(lrn_ranger)), 
                            list(task_basic_Z, task_basic_N), 
                            cv_desc, measures),
  
  # tuning rpart
  par_rpart = tune_rpart(task_basic_N, measures, cv_desc),
  lrn_rpart_T = makeLearner("classif.rpart", id = "rpart_tuned", predict.type = "prob", par.vals = par_rpart),
  lrns_wb_T = c(lrns_wb, list(lrn_rpart_T)),
  
  # 1a - normalization
  data_normalized_Z = normalize_df(data_basic_Z),
  data_normalized_N = normalize_df(data_basic_N),
  task_normalization_Z = makeClassifTask("task_normalization_Z", data_normalized_Z, "TARGET", blocking = cv_inds_Z),
  task_normalization_N = makeClassifTask("task_normalization_N", data_normalized_N, "TARGET", blocking = cv_inds_N),
  bench_normalization = benchmark(lrns_wb_T, 
                                  list(task_normalization_Z, 
                                       task_normalization_N), 
                                  cv_desc, measures),
  
  # 1b - outlier reduction
  data_outliers_reduced_Z = reduce_outliers(data_basic_Z),
  data_outliers_reduced_N = reduce_outliers(data_basic_N),
  task_outliers_reduced_Z = makeClassifTask("task_outliers_reduced_Z", data_outliers_reduced_Z, "TARGET", blocking = cv_inds_Z),
  task_outliers_reduced_N = makeClassifTask("task_outliers_reduced_N", data_outliers_reduced_N, "TARGET", blocking = cv_inds_N),
  bench_outliers_reduced = benchmark(lrns_wb_T, 
                            list(task_outliers_reduced_Z, 
                                 task_outliers_reduced_N), 
                            cv_desc, measures),
  
  # 1c - logarithm
  data_logged_Z = apply_log(data_basic_Z),
  data_logged_N = apply_log(data_basic_N),
  task_logged_Z = makeClassifTask("task_logged_Z", data_logged_Z, "TARGET", blocking = cv_inds_Z),
  task_logged_N = makeClassifTask("task_logged_N", data_logged_N, "TARGET", blocking = cv_inds_N),
  bench_logged = benchmark(lrns_wb_T, 
                           list(task_logged_Z, 
                                task_logged_N), 
                           cv_desc, measures),
  
  # 2a - outlier reduction & normalization
  data_outliers_reduced_and_normalized_Z = normalize_df(data_outliers_reduced_Z),
  data_outliers_reduced_and_normalized_N = normalize_df(data_outliers_reduced_N),
  task_outliers_reduced_and_normalized_Z = makeClassifTask("task_outliers_reduced_and_normalized_Z", data_outliers_reduced_and_normalized_Z, "TARGET", blocking = cv_inds_Z),
  task_outliers_reduced_and_normalized_N = makeClassifTask("task_outliers_reduced_and_normalized_N", data_outliers_reduced_and_normalized_N, "TARGET", blocking = cv_inds_N),
  bench_outliers_reduced_and_normalized = benchmark(lrns_wb_T,
                                                    list(task_outliers_reduced_and_normalized_Z,
                                                         task_outliers_reduced_and_normalized_N),
                                                    cv_desc, measures),
  
  # 2b - logarithm & outlier reduction
  data_logged_and_outliers_reduced_Z = reduce_outliers(data_logged_Z),
  data_logged_and_outliers_reduced_N = reduce_outliers(data_logged_N),
  task_logged_and_outliers_reduced_Z = makeClassifTask("task_logged_and_outliers_reduced_Z", data_logged_and_outliers_reduced_Z, "TARGET", blocking = cv_inds_Z),
  task_logged_and_outliers_reduced_N = makeClassifTask("task_logged_and_outliers_reduced_N", data_logged_and_outliers_reduced_N, "TARGET", blocking = cv_inds_N),
  bench_logged_and_outliers_reduced = benchmark(lrns_wb_T,
                                                    list(task_logged_and_outliers_reduced_Z,
                                                         task_logged_and_outliers_reduced_N),
                                                    cv_desc, measures),
  

  # 3a - funModelling gain-ratio discretization
  data_discretized_gr = gr_disc(data_outliers_reduced_and_normalized_Z),
  task_discretized_gr = makeClassifTask("task_discretized_gr", data_discretized_gr, "TARGET", blocking = cv_inds_Z),
  bench_discretized_gr = benchmark(lrns_wb_T,
                     task_discretized_gr,
                     cv_desc, measures),
  
  # 3a - funModelling gain-ratio discretization
  data_discretized_rsafe = transform_rsafe(data_outliers_reduced_and_normalized_Z),
  task_discretized_rsafe = makeClassifTask("task_discretized_rsafe", data_discretized_rsafe, "TARGET", blocking = cv_inds_Z),
  bench_discretized_rsafe = benchmark(lrns_wb_T,
                                   task_discretized_rsafe,
                                   cv_desc, measures),
  
  
  # 2a <- current state
  
  # 4a - create new features selected by ranger
  variables_ranger = read.csv("data/variables_ranger.csv", stringsAsFactors=FALSE)$x,
  new_features_ranger = model_by_finded_variables(data_outliers_reduced_Z, variables_ranger, cv_inds_Z, cv_desc, lrn_ranger, lrns_wb_T, measures),
  new_features_ranger_improved = model_by_finded_variables(data_outliers_reduced_Z, new_features_ranger[[4]], cv_inds_Z, cv_desc, lrn_ranger, lrns_wb_T, measures),
  
  # 4b - create new features selected by ranger
  variables_rpart = read.csv("data/variables_rpart.csv", stringsAsFactors=FALSE)$x,
  new_features_rpart = model_by_finded_variables(data_outliers_reduced_Z, variables_rpart, cv_inds_Z, cv_desc, lrn_rpart_T, lrns_wb_T, measures),
  new_features_rpart_improved = model_by_finded_variables(data_outliers_reduced_Z, new_features_rpart[[4]], cv_inds_Z, cv_desc, lrn_rpart_T, lrns_wb_T, measures),
  
  # 4 - SMOTE algorithm
  bench_smote = calculate_smote(task_outliers_reduced_and_normalized_Z, 
                                    "task_smote", 1, lrns_wb_T, measures)
)
