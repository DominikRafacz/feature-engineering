installed <- rownames(installed.packages())

if (!"drake" %in% installed) install.packages("drake")
if (!"OpenML" %in% installed) install.packages("OpenML")
if (!"visNetwork" %in% installed) install.packages("visNetwork")
if (!"lubridate" %in% installed) install.packages("lubridate")
if (!"caret" %in% installed) install.packages("caret")
if (!"dplyr" %in% installed) install.packages("dplyr")
if (!"xgboost" %in% installed) install.packages("xgboost")
if (!"mlr" %in% installed) install.packages("mlr")
if (!"ranger" %in% installed) install.packages("ranger")
if (!"gbm" %in% installed) install.packages("gbm")

rm(installed)
