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

ggplot(data) + 
  geom_col(aes(y=result, x=model, fill=reorder(method, 3:1))) +
  geom_text(aes(x = model, y = cumsum(result) - result / 2, label = reorder(method, 3:1))) +
  ylab("improvement") +
  geom_abline(slope = 0, intercept = 100) +
  geom_abline(slope = 0, intercept = 0) +
  xlab("") +
  ggtitle("AUC improvement relative to the base difference") +
  scale_fill_manual(values = c("#f8961e", "#f9c74f", "#90be6d")) +
  geom_text(aes(x = 0.5, y = 102, label = "ranger result")) +
  geom_text(aes(x = 0.54, y = 2, label = "base rpart result")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.line.x = element_blank(), 
        rect = element_blank(), 
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())

