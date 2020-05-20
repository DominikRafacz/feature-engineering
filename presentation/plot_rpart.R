rpart_base <- 0.5000000
ranger <- 0.7916399

scaling <- function(value) {
  rpart_base <- 0.5000000
  ranger <- 0.7916399
  value / (ranger-rpart_base) * 100
}

rpart_tuning <- 0.7369539
outliers_and_norm <- 0.7394017
smote <- 0.804

scaling(outliers_and_norm-0.5)

data <- data.frame(values=c(rpart_tuning-rpart_base, outliers_and_norm-rpart_tuning, smote-outliers_and_norm))
data[["result"]] <- scaling(data$values)
data[["model"]] <- "rpart"
data[["method"]] <- c("tuning", "outliers reduction and normalization", "smote")

ggplot(data) + geom_col(aes(y=result, x=model, fill=method))
?geom_col

