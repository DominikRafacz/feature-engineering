library(OpenML)
library(DataExplorer)
library(funModeling)
library(dplyr)


JM1 <- getOMLDataSet(data.id = 1053)

data <- na.omit((JM1)$data)
colnames(data) <- c("MC_line_count","MC_cyclomatic","MC_essential","MC_design","num_oo","volume","program_length","difficulty","inteligence",
                    "effort", "b", "t","line_code", "line_comment","line_blank","line_code_and_comment","unique_operators","unique_operands",
                    "total_operators","total_operands","flow_graph","TARGET")
levels(data$TARGET) <- c(0, 1)
data
write.csv(data, file = "JM1_renamed.csv",row.names = FALSE)
View(data)
summary(data)
introduce(data = data)
plot_intro(data)
plot_missing(data)
pc <- plot_correlation(data)
plot_qq(data)
plot_bar(data)
plot_histogram(data)
View(df_status(data))
describe(data)
freq(data)
correlation_table(data, target = "line_code")
# to drop: t, b
plot_num(data %>% select(line_code), bins=50)
plot_num(data %>% select(line_code) %>% mutate(line_code=log(line_code)), bins = 50)
plot_num(data %>% select(difficulty) %>% mutate(difficulty=log(difficulty)), bins = 30)

data2 <- data %>% filter(line_code>0)
View(df_status(data2))

plot_num(data %>% mutate(lOCode=log(lOCode+1))%>%select(lOCode) ,bins=100)
plot_prcomp(data %>% select(-defects))

cross_plot(data, c("line_code"), target = "TARGET",)
plot_boxplot(data %>% mutate(MC_line_count=log(MC_line_count))%>% select(MC_line_count, TARGET), by="TARGET")
