library(dplyr)
library(scales)

transactions <- read.csv("Transactions.csv", stringsAsFactors = FALSE)
transactions <- transactions[complete.cases(transactions), ]
transactions$trans_dt <- as.Date(transactions$trans_dt, "%m/%d/%Y")

#####

transactions %>% group_by(prod_cat) %>% 
  summarize(year_total = sum(amount), 
            average_transaction = format(mean(amount), digits = 4, nsmall = 2)) %>% View(title = "1")

#####

table_A <- transactions %>% 
  mutate(transaction_level = ifelse(amount >= 1000, "high", 
                                    ifelse(amount >= 100, "medium", "low"))) 

table_B <- table_A %>% group_by(transaction_level) %>%
  summarize("Percent CC" = format(percent(sum(payment_method == "CC") / nrow(table_A)), digits = 3, nsmall = 2),
            "Percent A" = format(percent(sum(prod_cat == "A") / nrow(table_A)),digits = 3, nsmall = 2),
            "Percent B" = format(percent(sum(prod_cat == "B") / nrow(table_A)),digits = 3, nsmall = 2),
            "Percent C" = format(percent(sum(prod_cat == "C") / nrow(table_A)),digits = 3, nsmall = 2),
            "Percent D" = format(percent(sum(prod_cat == "D") / nrow(table_A)),digits = 3, nsmall = 2)
  ) 

table_B$transaction_level <- factor(table_B$transaction_level, levels = c("low", "medium", "high"), ordered = TRUE)


table_B[order(table_B$transaction_level),] %>% View(title = "2")

#####

table_3 <- transactions %>% 
  mutate(month = format(trans_dt, "%m")) %>% group_by(month)

table_4 <- table_3 %>% summarize("Total Sales" = format(sum(amount)))

table_5 <- table_3 %>% summarise("Total CC" = format(sum(amount[payment_method == "CC"])))

table_6 <- table_3 %>% summarize("Cat_A_Total" = sum(prod_cat == "A"),
                                 "Cat_B_Total" = sum(prod_cat == "B"),
                                 "Cat_C_Total" = sum(prod_cat == "C"),
                                 "Cat_D_Total" = sum(prod_cat == "D") 
)

table_6 <- table_6 %>% 
  mutate(Top_Total = pmax(`Cat_A_Total`, `Cat_B_Total`, `Cat_C_Total`, `Cat_D_Total`),
         Top_Category = ifelse(`Cat_A_Total` == Top_Total, "A",
                               ifelse(`Cat_B_Total` == Top_Total, "B",
                                      ifelse(`Cat_C_Total` == Top_Total, "C", "D"))))

table_6 <- table_6 %>% select(month, Top_Category)

table_4$Total_CC <- table_5$`Total CC`
table_4$Top_Category <- table_6$Top_Category

View(table_4, title = "3")