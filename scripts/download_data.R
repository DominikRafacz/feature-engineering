
download_data <- function(id, name) {
  JM1 <- getOMLDataSet(data.id = id)
  
  data <- na.omit((JM1)$data)
  colnames(data) <- c("MC_line_count","MC_cyclomatic","MC_essential","MC_design","num_oo","volume","program_length","difficulty","inteligence",
                      "effort", "b", "t","line_code", "line_comment","line_blank","line_code_and_comment","unique_operators","unique_operands",
                      "total_operators","total_operands","flow_graph","TARGET")
  levels(data$TARGET) <- c(0, 1)
  data["source"] <- name
  return(data)
}

download_data_special <- function(id, name) {
  JM1 <- getOMLDataSet(data.id = id)
  
  data <- na.omit((JM1)$data)
  colnames(data) <- c("MC_line_count","MC_cyclomatic","MC_essential","MC_design","num_oo","volume","program_length","difficulty","inteligence",
                      "effort", "b", "t","line_code", "line_comment","line_code_and_comment","line_blank","unique_operators","unique_operands",
                      "total_operators","total_operands","flow_graph","TARGET")
  levels(data$TARGET) <- c(0, 1)
  data["source"] <- name
  return(data)
}

prepare_CM1 <- function() {
  data <- read.csv("data/CM1.csv")
  levels(data$TARGET) <- c(0, 1)
  data["source"] <- "CM1"
  return(data)
}

datasets <- list()
datasets[[1]] <- download_data(1053, "JM1")
datasets[[2]] <- download_data(1067, "KC1")
datasets[[3]] <- download_data(1063, "KC2")
datasets[[4]] <- download_data_special(1068, "PC1")
datasets[[5]] <- prepare_CM1()

dataset <- rbind(datasets[[1]], datasets[[2]], datasets[[3]], datasets[[4]], datasets[[5]])
write.csv(dataset, file = "data/data_concataned.csv",row.names = FALSE)

#KC1 <- getOMLDataSet(data.id = 1068)
#data <- na.omit((KC1)$data)
#colnames(data)
#JM1 <- getOMLDataSet(data.id = 1053)
#colnames(JM1$data)
