fix_target <- function(dat) {
  dat$TARGET = as.factor(c(`0` = 0, `1` = 1, false = 0, true = 1)[dat$TARGET])
  dat
}

impute_median <- function(dat) {
  as.data.frame(lapply(dat, function(column) {
    if (is.numeric(column))
      ifelse(is.na(column), median(column, na.rm = TRUE), column)
    else column
  }))
}

one_hot_encode <- function(dat) {
  data.frame(lapply(dat, function(column) {
    if (is.character(column)) 
      setNames(do.call(cbind, lapply(unique(column), function(value)
          as.numeric(column == value)
        )), unique(column))
    else column
  }))
}

reduce_outliers <- function(data){
  num_var <- status(data) %>% filter(type=='numeric' & p_zeros<0.5) %>% select(variable) %>% unlist()
  data_tukey <- prep_outliers(data,num_var, type='stop', method = "tukey")
  num_var2 <- status(data) %>% filter(type=='numeric' & p_zeros>=0.5) %>% select(variable) %>% unlist()
  data_bt <- prep_outliers(data_tukey,num_var2, type='stop', method = "bottom_top", bottom_percent=0, top_percent = 0.01)
}

# Variable discretizatio
discretize <- function(data, n_bins){
  db <- discretize_get_bins(data, n_bins = n_bins)
  discretize_df(data, db)
}

# Variable discretization based on gain ratio maximization
gr_disc <- function(data){
  num_var <- status(data) %>% filter(type=='numeric' & unique>10) %>% select(variable) %>% unlist()
  l <- lapply(num_var, function(col){
    input <- data[,col]
    target <- data$TARGET
    data[,col] <- discretize_rgr(input, target, max_n_bins = 10)
  })
  data[,num_var] <- l
  data
}

apply_log <- function(data){
  num_var <- status(data) %>% filter(type=='numeric' & unique>10) %>% select(variable) %>% unlist()
  data%>% select(-source.1,-source.2, - source.3, -source.4, -source.5) %>% mutate_at(vars(num_var),function(x)log(x+1))
}