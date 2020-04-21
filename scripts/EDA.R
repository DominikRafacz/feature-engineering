library(OpenML)
library(DataExplorer)
library(funModeling)
library(dplyr)


# renaming columns --------------------------------------------------------
JM1 <- getOMLDataSet(data.id = 1053)

data <- na.omit((JM1)$data)
colnames(data) <- c("MC_line_count","MC_cyclomatic","MC_essential","MC_design","num_oo","volume","program_length","difficulty","inteligence",
                    "effort", "b", "t","line_code", "line_comment","line_blank","line_code_and_comment","unique_operators","unique_operands",
                    "total_operators","total_operands","flow_graph","TARGET")
levels(data$TARGET) <- c(0, 1)
write.csv(data, file = "data/JM1_renamed.csv",row.names = FALSE)


# EDA ---------------------------------------------------------------------
colnames(data)
View(status(data))
di <- data_integrity(data)
summary(di)
print(di)
View(profiling_num(data))
cross_plot(data, target = "TARGET")
summary(data)
introduce(data = data)
plot_intro(data)

# McCabe's measures correlation -------------------------------------------

plot_correlation(data %>% select(MC_line_count,MC_cyclomatic,MC_essential,MC_design,TARGET))
plot_correlation(data %>% select(-MC_line_count,-MC_cyclomatic,-MC_essential,-MC_design))

# strong correlations -----------------------------------------------------

correlation_table(data, "total_operators")
plot(data$total_operators,data$num_oo)
plot(data$b, data$volume)
plot(data$t, data$effort)
plot(data$total_operators, data$volume)

plot_histogram(data)
plot_boxplot(data,by = "TARGET")
freq(data)

# columns discretization --------------------------------------------------
db <- discretize_get_bins(data, n_bins = 10)
data_discrete <- discretize_df(data, db)
cross_plot(data_discrete, target = "TARGET")
plot_bar(data_discrete)

# outliers ----------------------------------------------------------------
describe(data$MC_line_count)
count(data[data$MC_line_count>300,])
plot_boxplot(data[data$MC_line_count<100,],by = "TARGET")
plot_histogram(data[data$MC_line_count<100,])
describe(data_po$flow_graph)
# to test
hampel_outlier(data$flow_graph)
data_po <- prep_outliers(data,input = c('flow_graph'), type='stop', method='tukey')

# to drop: t, b

# data transformation -----------------------------------------------------
plot_num(data %>% select(line_code), bins=50)
plot_num(data %>% select(line_code) %>% mutate(line_code=log(line_code)), bins = 50)
plot_num(data %>% select(difficulty) %>% mutate(difficulty=log(difficulty)), bins = 30)

data2 <- data %>% filter(line_code>0)
View(df_status(data2))


cross_plot(data, c("line_code"), target = "TARGET",)
plot_boxplot(data %>% mutate(MC_line_count=log(MC_line_count))%>% select(MC_line_count, TARGET), by="TARGET")
