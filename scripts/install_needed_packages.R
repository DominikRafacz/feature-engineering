installed <- rownames(installed.packages())

for (package in c("drake", 
                  "OpenML",
                  "farff",
                  "visNetwork",
                  "lubridate",
                  "DALEX",
                  "caret",
                  "dplyr",
                  "stringi",
                  "mlr",
                  "mlrMBO",
                  "ranger",
                  "funModeling",
                  "kknn",
                  "DiceKriging",
                  "rgenoud",
                  "devtools",
                  "smotefamily"
                  )) {
  if (!package %in% installed) install.packages(package)
}

if (!"rSAFE" %in% installed) devtools::install_github("ModelOriented/rSAFE")

rm(installed)
