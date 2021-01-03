library(tidyr)

data <- read.csv("Forecast HC (Realistic Upside Case) 2020-04-22.csv")
data_long <- gather(data,date,count, 'Jan.20':'Dec.20', factor_key = TRUE)
