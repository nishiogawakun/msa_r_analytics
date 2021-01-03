library(dplyr)

transactions <- read.csv("Transactions.csv", stringsAsFactors = FALSE)
transactions <- transactions[complete.cases(transactions), ]
transactions$trans_dt <- as.Date(transactions$trans_dt, "%m/%d/%Y")

transactions %>% group_by(prod_cat) %>% 
  summarize(year_total = sum(amount), 
            average_transaction = format(mean(amount), digits = 4, nsmall = 2)) %>% View()