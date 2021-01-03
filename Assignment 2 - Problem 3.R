library(dplyr)
library(tidyr)

transactions <- read.csv("Transactions.csv", stringsAsFactors = FALSE)
transactions <- transactions[complete.cases(transactions), ]
transactions$trans_dt <- as.Date(transactions$trans_dt, "%m/%d/%Y")

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

View(table_4)