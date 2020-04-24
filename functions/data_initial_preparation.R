download_data <- function(id, name) {
  dat <- getOMLDataSet(data.id = id)$data
  colnames(dat) <- c(loc = "MC_line_count",
                     `v.g.` = "MC_cyclomatic",
                     `ev.g.` = "MC_essential",
                     `iv.g.` = "MC_design",
                     n = "num_oo",
                     v ="volume",
                     l = "program_length",
                     d = "difficulty",
                     i = "inteligence",
                     e = "effort", 
                     b = "b", 
                     t = "t",
                     locode = "line_code", 
                     locomment = "line_comment",
                     loblank = "line_blank",
                     loccodeandcomment = "line_code_and_comment",
                     locodeandcomment = "line_code_and_comment",
                     uniq_op = "unique_operators",
                     uniq_opnd = "unique_operands",
                     total_op = "total_operators",
                     total_opnd = "total_operands",
                     branchcount = "flow_graph",
                     defects = "TARGET",
                     problems = "TARGET")[tolower(colnames(dat))]
  levels(dat$TARGET) <- c(0, 1)
  dat["source"] <- name
  dat
}

read_data_from_disk <- function(path, name) {
  dat <- read.csv(path)
  levels(dat$TARGET) <- c(0, 1)
  dat["source"] <- name
  dat
}

obtain_all_data <- function() {
  do.call(bind_rows, list(
    download_data(1053, "JM1"),
    download_data(1067, "KC1"),
    download_data(1063, "KC2"),
    download_data(1068, "PC1"),
    read_data_from_disk("data/CM1.csv", "CM1")
  ))
}

split_data <- function(data_raw) {
  set.seed(1998)
  size <- floor(0.8 * nrow(data_raw))
  train_indexes <- sample(seq_len(nrow(data_raw)), size = size)
  
  data_train <- data_raw[train_indexes, ]
  data_test <- data_raw[-train_indexes, ]
  return(list(data_train = data_train, 
              data_test = data_test))
}