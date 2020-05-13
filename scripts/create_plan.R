plan <- drake_plan(
  data_raw = obtain_all_data(),
  data_raw_fixed = fix_target(data_raw),
  
  save_data = write.csv(data_raw_fixed, 
                        file_out("data/data_concataned.csv"), row.names = FALSE),
  data_ohe = one_hot_encode(data_raw_fixed),
  data_imp = impute_median(data_ohe),
  data_red = remove_advanced_measures(data_imp),
  
  data_norm = normalize_df(data_red),
  data_rsafe = transform_rsafe(data_red),
  data_out = reduce_outliers(data_red),
  data_out_norm = normalize_df(data_out),
  data_out_rsafe = transform_rsafe(data_out),
  data_log = apply_log(data_out),
  data_gr = gr_disc(data_out),
  
  # data without halstead measures 
  data_no_hal_red = remove_halstead_measures(data_out),
  
  # data without rows with line_code = 0
  data_no_zero_LC = data_raw_fixed %>% filter(!(line_code==0)),
  data_no_zero_LC_ohe = one_hot_encode(data_no_zero_LC),
  data_no_zero_LC_imp = impute_median(data_no_zero_LC_ohe),
  data_no_zero_LC_red = remove_advanced_measures(data_no_zero_LC_imp),
  data_no_zero_LC_norm = normalize_df(data_no_zero_LC_red),
  data_no_zero_LC_out = reduce_outliers(data_no_zero_LC_red),
  data_no_zero_LC_out_rsafe = transform_rsafe(data_no_zero_LC_out),
  data_no_zero_LC_dis = discretize(data_no_zero_LC_out,10),
  data_no_zero_LC_gr = gr_disc(data_no_zero_LC_out),
  save_data_no_zero_LC = write.csv(data_no_zero_LC_red, 
                        file_out("data/data_no_zero_LC.csv"), row.names = FALSE),
  
  cv_inds = generate_cv_inds(data_red),
  cv_inds_no_zero_LC = generate_cv_inds(data_no_zero_LC_red),
  cv_desc = makeResampleDesc("CV", fixed = TRUE, iters = 5),
  
  measures = list(auc, acc, ppv, tpr, f1),
  
  task_0 = makeClassifTask("task_0", data_red, "TARGET", blocking = cv_inds),
  
  lrn_0_ranger = makeLearner("classif.ranger", predict.type = "prob"),
  
  lrn_0_logreg = makeLearner("classif.logreg", predict.type = "prob"),
  lrn_0_rpart = makeLearner("classif.rpart", predict.type = "prob"),
  lrn_0_knn = makeLearner("classif.kknn", predict.type = "prob"),
  
  bench_0 = benchmark(list(lrn_0_ranger,
                           lrn_0_knn,
                           lrn_0_rpart,
                           lrn_0_logreg), task_0, cv_desc, measures),
  
  # data without rows with line_code = 0
  task_2 = makeClassifTask("task_2", data_no_zero_LC_red, "TARGET", blocking = cv_inds_no_zero_LC),
  bench_2 = benchmark(list(lrn_0_ranger,
                           lrn_0_knn,
                           lrn_0_rpart,
                           lrn_0_logreg), task_2, cv_desc, measures),
  
  par_rpart = tune_rpart(task_0, measures, cv_desc),
  lrn_1_rpart = makeLearner("classif.rpart",
                            id = "rpart_tuned",
                            predict.type = "prob", 
                            par.vals = par_rpart),
  
  # data with reduced outliers and without Halstead's measures
  task_3 = makeClassifTask("task_3", data_no_hal_red, "TARGET", blocking = cv_inds),
  bench_3 = benchmark(list(lrn_0_knn,
                           lrn_0_rpart,
                           lrn_1_rpart,
                           lrn_0_logreg), task_3, cv_desc, measures),
  
  # data with reduced outliers
  task_4 = makeClassifTask("task_4", data_out, "TARGET", blocking = cv_inds),
  bench_4 = benchmark(list(lrn_0_knn,
                           lrn_0_rpart,
                           lrn_1_rpart,
                           lrn_0_logreg), task_4, cv_desc, measures),
  
  # data without rows with line_code = 0 and with reduced outliers
  task_5 = makeClassifTask("task_5", data_no_zero_LC_out, "TARGET", blocking = cv_inds_no_zero_LC),
  bench_5 = benchmark(list(lrn_0_knn,
                           lrn_0_rpart,
                           lrn_1_rpart,
                           lrn_0_logreg), task_5, cv_desc, measures),
  
  # data with reduced outliers and logatrithm of numeric columns
  task_6 = makeClassifTask("task_6", data_log, "TARGET", blocking = cv_inds),
  bench_6 = benchmark(list(lrn_0_knn,
                           lrn_0_rpart,
                           lrn_1_rpart,
                           lrn_0_logreg), task_6, cv_desc, measures),
  
  # data without rows with line_code = 0 and with discretized columns(10 bins)
  task_7 = makeClassifTask("task_7", data_no_zero_LC_dis, "TARGET", blocking = cv_inds_no_zero_LC),
  bench_7 = benchmark(list(lrn_0_knn,
                           lrn_0_rpart,
                           lrn_1_rpart,
                           lrn_0_logreg), task_7, cv_desc, measures),
  
  # data without rows with line_code = 0 and with gain-ratio discretized columns
  task_8 = makeClassifTask("task_8", data_no_zero_LC_gr, "TARGET", blocking = cv_inds_no_zero_LC),
  bench_8 = benchmark(list(lrn_0_knn,
                           lrn_0_rpart,
                           lrn_1_rpart,
                           lrn_0_logreg), task_8, cv_desc, measures),
  
  # data with reduced outliers and gain-ratio discretized columns
  task_9 = makeClassifTask("task_9", data_gr, "TARGET", blocking = cv_inds),
  bench_9 = benchmark(list(lrn_0_knn,
                           lrn_0_rpart,
                           lrn_1_rpart,
                           lrn_0_logreg), task_9, cv_desc, measures),
  
  task_10 = undersample(task_9, 0.8),
  bench_10 = benchmark(list(lrn_0_knn,
                           lrn_0_rpart,
                           lrn_1_rpart,
                           lrn_0_logreg), task_10, cv_desc, measures),
  
  task_11 = oversample(task_10, 1.5),
  bench_11 = benchmark(list(lrn_0_knn,
                            lrn_0_rpart,
                            lrn_1_rpart,
                            lrn_0_logreg), task_11, cv_desc, measures),

  task_12 = smote(task_10, 1.5),
  bench_12 = benchmark(list(lrn_0_knn,
                            lrn_0_rpart,
                            lrn_1_rpart,
                            lrn_0_logreg), task_12, cv_desc, measures),
  # data with normalized variables
  task_13 = makeClassifTask("task_13", data_norm, "TARGET", blocking = cv_inds),
  bench_13 = benchmark(list(lrn_0_knn,
                           lrn_0_rpart,
                           lrn_1_rpart,
                           lrn_0_logreg), task_13, cv_desc, measures),
  # data with reduced outliers and normalized variables
  task_14 = makeClassifTask("task_14", data_out_norm, "TARGET", blocking = cv_inds),
  bench_14 = benchmark(list(lrn_0_knn,
                           lrn_0_rpart,
                           lrn_1_rpart,
                           lrn_0_logreg), task_14, cv_desc, measures),
  # data with variables from rSAFE
  task_15 = makeClassifTask("task_15", data_rsafe, "TARGET", blocking = cv_inds),
  bench_15 = benchmark(list(lrn_0_knn,
                            lrn_0_rpart,
                            lrn_1_rpart,
                            lrn_0_logreg), task_15, cv_desc, measures),
  # data with reduced outliers and variables from rSAFE
  task_16 = makeClassifTask("task_16", data_out_rsafe, "TARGET", blocking = cv_inds),
  bench_16 = benchmark(list(lrn_0_knn,
                            lrn_0_rpart,
                            lrn_1_rpart,
                            lrn_0_logreg), task_16, cv_desc, measures),
  # data without rows with line_code = 0, reduced outliers and variables from rsafe 
  task_17 = makeClassifTask("task_17", data_no_zero_LC_out_rsafe, "TARGET", blocking = cv_inds_no_zero_LC),
  bench_17 = benchmark(list(lrn_0_knn,
                           lrn_0_rpart,
                           lrn_1_rpart,
                           lrn_0_logreg), task_17, cv_desc, measures),
  task_18 = makeClassifTask("task_18", data_no_zero_LC_norm, "TARGET", blocking = cv_inds_no_zero_LC),
  bench_18 = benchmark(list(lrn_0_knn,
                            lrn_0_rpart,
                            lrn_1_rpart,
                            lrn_0_logreg), task_18, cv_desc, measures)
)
