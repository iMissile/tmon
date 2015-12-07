library(readr)
library(dplyr)
library(lubridate)
library(magrittr)

data <- read_csv("LoadProfile_data.csv", col_names = FALSE)

# добавляем последнее наблюдение, чтобы неделя была целая
data[10080,] <- (data[10079,] + data[1,]) / 2

data %<>% transmute(
  timestamp = seq(ymd("2015-12-07"), ymd_hm("2015-12-13 23:59"), by = "1 min"),
  value = X2
)

write_csv(data, path = "LoadProfile_final.csv")
