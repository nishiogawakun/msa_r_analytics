library(dplyr)
library(scales)

transactions <- read.csv("Transactions.csv", stringsAsFactors = FALSE)
transactions <- transactions[complete.cases(transactions), ]
transactions$trans_dt <- as.Date(transactions$trans_dt, "%m/%d/%Y")

table_1 <- transactions %>% 
  mutate(transaction_level = ifelse(amount >= 1000, "high", 
                                    ifelse(amount >= 100, "medium", "low"))) 

table_2 <- table_1 %>% group_by(transaction_level) %>%
  summarize("Percent CC" = format(percent(sum(payment_method == "CC") / nrow(table_1)), digits = 3, nsmall = 2),
            "Percent A" = format(percent(sum(prod_cat == "A") / nrow(table_1)),digits = 3, nsmall = 2),
            "Percent B" = format(percent(sum(prod_cat == "B") / nrow(table_1)),digits = 3, nsmall = 2),
            "Percent C" = format(percent(sum(prod_cat == "C") / nrow(table_1)),digits = 3, nsmall = 2),
            "Percent D" = format(percent(sum(prod_cat == "D") / nrow(table_1)),digits = 3, nsmall = 2)
            ) 

table_2$transaction_level <- factor(table_2$transaction_level, levels = c("low", "medium", "high"), ordered = TRUE)


table_2[order(table_2$transaction_level),] %>% View()
