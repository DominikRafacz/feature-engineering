find_important_features <- function(learner, data_red, cv_inds, pack=1, from=0, to=10000) {
  data_gen <- create_features_for_data(data_red, c("line_code",
                                                   "line_comment",
                                                   "line_blank",
                                                   "line_code_and_comment",
                                                   "unique_operators",
                                                   "unique_operands",
                                                   "total_operators",
                                                   "total_operands",
                                                   "flow_graph"), pack=pack, from=from, to=to)
  if (ncol(data_gen) == ncol(data_red) + 1) {
    return(c())
  }
  print(list("data_gen", ncol(data_gen)))
  task_1 <- makeClassifTask("task_1", filter_out_infinite_and_nan(data_gen),
                            "TARGET", blocking = cv_inds)
  learned <- mlr::train(learner, task_1)
  pred <-  function(object, newdata) {pred <- predict(object, newdata=newdata)
    response <- pred$data[,3]
    return(as.numeric(as.character(response)))}
  explained <- DALEX::explain(learned, data=select(filter_out_infinite_and_nan(data_gen), -TARGET),
                              y=as.numeric(as.character(data_gen$TARGET)), label="tree",
                              predict_function = pred, colorize = FALSE, verbose = F)
  variable_imp <- DALEX::variable_importance(explained, loss_function = DALEX::loss_one_minus_auc, n_sample=50)

  v_imp_df <- data.frame(variable_imp)
  ordered <- order(v_imp_df$dropout_loss, decreasing=TRUE)
  v_imp_df <- v_imp_df[ordered, ][1:10, ]
  unique_variables <- as.character(unique(v_imp_df$variable))

  return(reverse_names_substition(unique_variables))
}

find_important_features_for_all_packs <- function(learner, data_red, cv_inds) {
  variables <- c()
  for(i in 1:13) {
    for (j in 1:10) {
      variables <- c(variables, find_important_features(learner, data_red, cv_inds, pack=i, from=(j-1)*1000, to=j*1000))
      # variables <- c(variables, find_important_features(learner, data_red, cv_inds, pack=i))
      print("Ended: ")
      print(i)
      print(j)
      write.csv(variables, paste0("data/variables_ranger",as.character(i),"_",as.character(j),".csv"))
    }
  }
  return(variables)
}

save_important_variables <- function(pack) {
  learner <- readd(lrn_rpart_T)
  data <- readd(data_outliers_reduced_Z)
  data <- remove_normalization_information(data)
  cv_inds <- readd(cv_inds)
  if(pack=="all") {
    variables <- find_important_features_for_all_packs(learner, data, cv_inds)
  }
  else {
    variables <- find_important_features(learner, data, cv_inds, pack=pack)
  }
  write.csv(variables, "data/variables_ranger.csv")
}

remove_normalization_information <- function(data) {
  for (colname in setdiff(colnames(data), c("TARGET"))) {
    data[[colname]] <- as.numeric(data[[colname]])
  }
  return(data)
}

# data <- readd(data_outliers_reduced_Z)
# learner <- readd(lrn_ranger)
# learners <- readd(lrns_wb_T)
# measures <- readd(measures)
# cv_inds <- readd(cv_inds)
# cv_desc <- readd(cv_desc)
# # find_important_features(learner, data, cv_inds, pack=0, from=1000, to=1100)
# # save_important_variables("all")
# # variables <- find_important_features(learner, data, cv_inds, pack=pack)
# learner <- readd(lrn_ranger)
# variables <- read.csv("data/variables_ranger.csv", stringsAsFactors=FALSE)$x
# bench_new_features <- model_by_finded_variables(data, variables, cv_inds, cv_desc, learner, learners, measures)
# plot(bench_new_features[[2]])
# bench_new_features[[3]]
# bench_new_features[[1]]
# 
# bench_new_features2 <- model_by_finded_variables(data, bench_new_features[[3]], cv_inds, cv_desc, learner, learners, measures)
# plot(bench_new_features2[[2]])
# bench_new_features2[[3]]
# bench_new_features2[[1]]

model_by_finded_variables <- function(data, variables, cv_inds, cv_desc, learner, learners, measures, aic_bic=FALSE) {
  variables <- setdiff(variables, colnames(data))
  variables <- setdiff(variables, c("_baseline_", "_full_model_"))
  data_gen <- create_features_by_expressions(data, variables)
  data_gen <- normalize_df(data_gen)
  data_gen <- remove_normalization_information(data_gen)
  task_0 <- makeClassifTask("task_0", data,
                            "TARGET", blocking = cv_inds)
  task_1 <- makeClassifTask("task_1", filter_out_infinite_and_nan(data_gen),
                            "TARGET", blocking = cv_inds)
  learned <- mlr::train(learner, task_1)
  pred <- function(object, newdata) {
    pred <- predict(object, newdata=newdata) 
    response <- pred$data[,3]
  return(as.numeric(as.character(response)))}
  explained <- DALEX::explain(learned, data=select(filter_out_infinite_and_nan(data_gen), -TARGET),
                              y=as.numeric(as.character(data_gen$TARGET)), label="tree",
                              predict_function = pred, colorize = FALSE, verbose = F)
  variable_imp <- DALEX::variable_importance(explained, loss_function = DALEX::loss_one_minus_auc)
  
  v_imp_df <- data.frame(variable_imp)
  ordered <- order(v_imp_df$dropout_loss, decreasing=TRUE)
  v_imp_df <- v_imp_df[ordered, ][1:30, ]
  unique_variables <- as.character(unique(v_imp_df$variable))
  
  if (aic_bic) {
    data_to_lm <- filter_out_infinite_and_nan(data_gen)
    data_to_lm$TARGET <- as.numeric(as.character(data_to_lm$TARGET))
    logreg_model <- glm("TARGET~.", data_to_lm, family='binomial')
    summary(logreg_model)
    aic <- step(logreg_model, direction="backward", k=2)
    cols <- stri_replace_all(stri_split_lines(stri_replace_all(aic$formula, "\n", fixed=" + "))[[3]], "", fixed=" ")
    cols <- setdiff(cols, c(""))
    bic <- step(logreg_model, direction="backward", k=log(nrow(data)-2))
    cols2 <- stri_replace_all(stri_split_lines(stri_replace_all(bic$formula, "\n", fixed=" + "))[[3]], "", fixed=" ")
    cols2 <- setdiff(cols2, c(""))
    task_2 <- makeClassifTask("task_2", data_gen[, c(cols, "TARGET")],
                              "TARGET", blocking = cv_inds)
    task_3 <- makeClassifTask("task_3", data_gen[, c(cols2, "TARGET")],
                              "TARGET", blocking = cv_inds)
    tasks <- list(task_0, task_1, task_2, task_3)
  }
  else {
    tasks <- list(task_0, task_1)
  }
  bench_2 <- benchmark(learners, tasks, cv_desc, measures)
  return(list(bench_2, data_gen, variable_imp, reverse_names_substition(unique_variables)))
}

#pdp  <- variable_effect(explained, variable = "name_pf_variable", type = 'partial_dependency')
  
# task_less <- makeClassifTask("task_less", select(filter_out_infinite_and_nan(data_gen), c(unique_variables, "TARGET")),
#                              "TARGET", blocking = readd(cv_inds))
# logreg_less <- mlr::train(lrn_0_logreg, task_less)
# explained_less <- DALEX::explain(logreg_less, data=select(filter_out_infinite_and_nan(data_gen), unique_variables),
#                             y=as.numeric(as.character(data_gen$TARGET)), label="tree",
#                             predict_function = pred, colorize = FALSE, verbose = F)
# variable_imp_less <- DALEX::variable_importance(explained_less, loss_function = DALEX::loss_root_mean_square)
# plot(variable_imp_less)



