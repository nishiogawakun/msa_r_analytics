library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)

bank_customers <- read.csv("bank_customers.csv", stringsAsFactors = FALSE)

bank_customers <- bank_customers[!(bank_customers$ZIP.Code < 10000),]
bank_customers$Income <- bank_customers$Income * 1000
bank_customers$CCAvg <- bank_customers$CCAvg * 1000

#Recode Education column from numeric to character where 
#1 represents "Undergrad," 2 represents "Graduate," and 3 represents "Advanced."
bank_customers$Education[bank_customers$Education == 1] <- "Undergrad"
bank_customers$Education[bank_customers$Education == 2] <- "Graduate"
bank_customers$Education[bank_customers$Education == 3] <- "Advanced"

#Make Education an ordered factor variable.
bank_customers$Education <- ordered(bank_customers$Education, levels = c("Undergrad", "Graduate", "Advanced"))
str(bank_customers)

#Create an Income_level column with "Low" representing below $50,000, 
#"Medium" representing $50,000 - $150,000 inclusive and "High" representing above $150,000. 
bank_customers <- bank_customers %>% 
  mutate(Income_level = ifelse(Income > 150000, "High", 
                                    ifelse(Income >= 50000, "Medium", "Low"))) 

bank_customers$Income_level <- ordered(bank_customers$Income_level, levels = c("Low", "Medium", "High"))
str(bank_customers)

#Create a facet wrap bar chart that shows the Average Age for each Income_level that is wrapped by Education.
p1 <- ggplot(bank_customers) + 
  stat_summary(aes(Income_level, Age), fun.y = "mean", geom = "bar") +
  facet_wrap(~Education) + theme_hc() + 
  theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) + 
  labs(title = "Average Age by Income and Educational Level")

print(p1)

#Create a scatter plot of average spending on credit cards (CCAvg) vs. income. 
#CCavg should be the y-variable while income should be the x-variable. 
#Use different point colors to represent the varying levels of education. 
p2 <- ggplot(bank_customers, aes(x = Income, y = CCAvg, col = Education)) + geom_point(alpha = 0.5) +
  theme_hc() + 
  theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) + 
  labs(title = "Average Credit Card Spend by Income and Educational Level")

print(p2)

#Create 3 linear regression models of CCavg (y) vs. Income (x) for each level of education. 
#Show the regression coefficients of each model.
model1 <- lm(CCAvg ~ Income, data = subset(bank_customers,Education == "Undergrad"))
print(coef(model1))
model2 <- lm(CCAvg ~ Income, data = subset(bank_customers,Education == "Graduate"))
print(coef(model2))
model3 <- lm(CCAvg ~ Income, data = subset(bank_customers,Education == "Advanced"))
print(coef(model3))

modelall <- lm(CCAvg ~ Income, data = bank_customers)
modelall_predict <- predict(modelall)
bank_customers_predict <- data.frame(bank_customers, modelall_predict, modelall$residuals)

Top_overestimated_CCAvg <- head(bank_customers_predict %>% 
                                   arrange(bank_customers_predict$modelall.residuals), 1)

#What was the customer ID and corresponding residual value of the customer with the most overestimated CCavg? 
print(Top_overestimated_CCAvg[,c(1, 11)])

Top_underestimated_CCAvg <- head(bank_customers_predict %>% 
                                arrange(desc(bank_customers_predict$modelall.residuals)), 1)

#What was the customer ID and corresponding residual value of the customer with the most underestimated CCavg?
print(Top_underestimated_CCAvg[,c(1, 11)])

bank_customers_predict <- bank_customers_predict %>% mutate(exp_dif = abs((modelall_predict - CCAvg) / CCAvg))

#How many customers had a predicted value within 10% (plus or minus) from their actual value?
print(sum(bank_customers_predict$exp_dif <= 0.1))
