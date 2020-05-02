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