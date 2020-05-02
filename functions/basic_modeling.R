generate_cv_inds <- function(dat, folds = 10) {
  inds <- 1:nrow(dat)
  positive_inds <- inds[dat$TARGET == 1]
  negative_inds <- inds[dat$TARGET == 0]
  
  perm <- c(sample(1:length(positive_inds), length(positive_inds)),
            sample(1:length(negative_inds), length(negative_inds)))
  
  cv_inds <- rep(1:5, ceiling(nrow(dat) / 5))[inds];
  factor(cv_inds[order(perm)])
}

filter_out_infinite_and_nan <- function(dat) {
  dat <- dat[, !unlist(lapply(dat, function(column) if (!is.numeric(column)) 0 else any(is.infinite(column))))]
  dat[, !unlist(lapply(dat, function(column) if (!is.numeric(column)) 0 else any(is.nan(column))))]

}