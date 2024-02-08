library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)


file_list <- list.files("./data/hobo_data/atmp_qp" ,pattern = "\\.csv$", full.names=TRUE)



pressure <- do.call(rbind, lapply(file_list, function(file) {
  read.csv(file, header = TRUE)  # Adjust read function as per your file type
}))

colnames(pressure)


write.csv(pressure, "./data/pressure_data.csv", row.names=TRUE)

pressure <- pressure %>% 
  rename("local.time" = "date_time_GMT4")


