smotize_task <- function(task, task_id, dup_size) {
  folds <- lapply(levels(task$blocking), function(fold) {
    dat <- task$env$data[task$blocking == fold, ]
    sources_unique <- lapply(c("source.1", "source.2", "source.3", "source.4", "source.5"), 
                             function(col) unique(dat[[col]]))
    gen_data <- SMOTE(dat[, names(dat) != "TARGET"], dat[, names(dat) == "TARGET"], dup_size = dup_size)
    gen_data <- rbind(gen_data$data, gen_data$syn_data)
    for (col in c("source.1", "source.2", "source.3", "source.4", "source.5")) {
      source_unique <- unique(dat[[col]])
      gen_data[[col]] <- ifelse(abs(gen_data[[col]] - source_unique[1, ]) <= abs(gen_data[[col]] - source_unique[2, ]),
                                source_unique[1, ],
                                source_unique[2, ])
    }
    colnames(gen_data)[colnames(gen_data) == "class"] <- "TARGET"
    gen_data
  })
  makeClassifTask(task_id, as.data.frame(do.call(rbind, folds)), "TARGET", blocking = factor(do.call(c, lapply(1:5, function(fold) rep(fold, nrow(folds[[fold]]))))))
}

calculate_smote <- function(task, task_id, dup_size, lrns, measures) {
  folds <- lapply(levels(task$blocking), function(fold) {
    dat <- task$env$data[task$blocking == fold, ]
    sources_unique <- lapply(c("source.1", "source.2", "source.3", "source.4", "source.5"), 
                             function(col) unique(dat[[col]]))
    gen_data <- SMOTE(dat[, names(dat) != "TARGET"], dat[, names(dat) == "TARGET"], dup_size = dup_size)
    gen_data$data$syntetic = 0
    gen_data$syn_data$syntetic = 1
    gen_data <- rbind(gen_data$data, gen_data$syn_data)
    for (col in c("source.1", "source.2", "source.3", "source.4", "source.5")) {
      source_unique <- unique(dat[[col]])
      gen_data[[col]] <- ifelse(abs(gen_data[[col]] - source_unique[1]) <= abs(gen_data[[col]] - source_unique[2]),
                                source_unique[1],
                                source_unique[2])
    }
    colnames(gen_data)[colnames(gen_data) == "class"] <- "TARGET"
    gen_data
  })
  do.call(rbind, lapply(1:length(folds), function(fold) {
    train_set <- as.data.frame(do.call(rbind, folds[-fold]))
    train_set <- train_set[, colnames(train_set) != "syntetic"]
    test_set <- folds[[fold]][folds[[fold]]$syntetic == 0, colnames(folds[[fold]]) != "syntetic"]
    tsk <- makeClassifTask(task_id, rbind(train_set, test_set), "TARGET")
    hi <- makeFixedHoldoutInstance(1:nrow(train_set), (nrow(train_set) + 1):(nrow(train_set) + nrow(test_set)), nrow(train_set) + nrow(test_set))
    as.data.frame(benchmark(lrns, tsk, hi, measures))
  })) %>%
    group_by(task.id, learner.id) %>%
    summarise_all(list(mean = mean))
}