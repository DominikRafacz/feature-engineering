installed <- rownames(installed.packages())

for (package in c("drake", 
                  "OpenML",
                  "farff",
                  "visNetwork",
                  "lubridate",
                  "caret",
                  "dplyr",
                  "stringi",
                  "mlr",
                  "xgboost",
                  "ranger",
                  "gbm",
                  "funModeling"
                  #"bartMachine"
                  )) {
  if (!package %in% installed) install.packages(package)
}


rm(installed)
