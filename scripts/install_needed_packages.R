installed <- rownames(installed.packages())

if (!"drake" %in% installed) install.packages("drake")
if (!"OpenML" %in% installed) install.packages("OpenML")
if (!"visNetwork" %in% installed) install.packages("visNetwork")
if (!"lubridate" %in% installed) install.packages("lubridate")

rm(installed)