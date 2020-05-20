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

remove_advanced_measures <- function(dat) {
  dat[, -(1:ncol(dat))[colnames(dat) %in% 
                               c("num_oo",             
                                 "volume", 
                                 "program_length",
                                 "difficulty",
                                 "inteligence",
                                 "effort",
                                 "b",                   #??
                                 "t")]]
}

remove_halstead_measures <- function(dat) {
  dat[, -(1:ncol(dat))[colnames(dat) %in% 
                         c("line_code",             
                           "line_comment", 
                           "line_blank",
                           "line_code_and_comment",
                           "unique_operators",
                           "unique_operands",
                           "total_operators",
                           "total_operands")]]
}

add_advanced_measures <- function(dat) {
  data.frame(scale(select(mutate(dat,
         volume = ifelse(is.nan((total_operators + total_operands) * log2(unique_operators + unique_operands)),
                         0, (total_operators + total_operands) * log2(unique_operators + unique_operands)),
         volume_minimal = (2 + unique_operands) * log2(2 + unique_operands),
         length = ifelse(is.nan(volume / (total_operators + total_operands)) | volume == 0, 1,
                         volume / (total_operators + total_operands)),
         difficulty = 1 / length,
         intelligence = length * volume,
         effort = volume / length
         ), -TARGET)), TARGET = dat$TARGET)
}