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