find_important_features <- function(learner, data_red, cv_inds, pack=1) {
  data_gen <- create_features_for_data(data_red, c("line_code",
                                                   "line_comment",
                                                   "line_blank",
                                                   "line_code_and_comment",
                                                   "unique_operators",
                                                   "unique_operands",
                                                   "total_operators",
                                                   "total_operands",
                                                   "flow_graph"), pack=pack)
  task_1 <- makeClassifTask("task_1", filter_out_infinite_and_nan(data_gen),
                            "TARGET", blocking = cv_inds)
  learned <- mlr::train(learner, task_1)
  pred <-  function(object, newdata) {pred <- predict(object, newdata=newdata)
    response <- pred$data[,3]
    return(as.numeric(as.character(response)))}
  explained <- DALEX::explain(learned, data=select(filter_out_infinite_and_nan(data_gen), -TARGET),
                              y=as.numeric(as.character(data_gen$TARGET)), label="tree",
                              predict_function = pred, colorize = FALSE, verbose = F)
  variable_imp <- DALEX::variable_importance(explained, loss_function = DALEX::loss_root_mean_square)

  v_imp_df <- data.frame(variable_imp)
  ordered <- order(v_imp_df$dropout_loss, decreasing=TRUE)
  v_imp_df <- v_imp_df[ordered, ][1:100, ]
  unique_variables <- as.character(unique(v_imp_df$variable))

  return(reverse_names_substition(unique_variables))
}

find_important_features_for_all_packs <- function(learner, data_red, cv_inds) {
  variables <- c()
  for(i in 2:7) {
    variables <- c(variables, find_important_features(learner, data_red, cv_inds, pack=i))
    print("Ended: ")
    print(i)
    write.csv(variables, paste0("data/variables",as.character(i),".csv"))
  }
  return(variables)
}

save_important_variables <- function(pack) {
  if(pack=="all") {
    variables <- find_important_features_for_all_packs(readd(lrn_0_logreg), readd(data_red), readd(cv_inds))
  }
  else {
    variables <- find_important_features(readd(lrn_0_logreg), readd(data_red), readd(cv_inds), pack=pack)
  }
  write.csv(variables, "data/variables.csv")
}

save_important_variables("all")

#pdp  <- variable_effect(explained, variable = "name_pf_variable", type = 'partial_dependency')
  
# task_less <- makeClassifTask("task_less", select(filter_out_infinite_and_nan(data_gen), c(unique_variables, "TARGET")),
#                              "TARGET", blocking = readd(cv_inds))
# logreg_less <- mlr::train(lrn_0_logreg, task_less)
# explained_less <- DALEX::explain(logreg_less, data=select(filter_out_infinite_and_nan(data_gen), unique_variables),
#                             y=as.numeric(as.character(data_gen$TARGET)), label="tree",
#                             predict_function = pred, colorize = FALSE, verbose = F)
# variable_imp_less <- DALEX::variable_importance(explained_less, loss_function = DALEX::loss_root_mean_square)
# plot(variable_imp_less)



