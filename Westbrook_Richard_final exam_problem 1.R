library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)

df <- read.csv("seasonal_data.csv", stringsAsFactors = FALSE)
df$mth <- as.Date(df$mth, "%m/%d/%Y")

#Show a table of average sales by month.
Avg_Sales_by_Month <- df %>% mutate(mth = format(mth, "%m")) %>% group_by(mth) %>% 
  summarize(Average_Sales = sum(sales) / n()) %>%View()

#Show a table of average sales by year.
Avg_Sales_by_Year <- df %>% mutate(yr = format(mth, "%Y")) %>% group_by(yr) %>% 
  summarize(Average_Sales = sum(sales) / n()) %>% View()


#Show a table that lists all years, the month that contained the maximum sales 
#amount for the entire year, and the corresponding amount.
Top_Sales_Month <- df %>% mutate(yr = format(mth, "%Y")) %>% mutate(mth = format(mth, "%m")) %>%
  group_by(yr) %>% summarize("January" = sum(sales[mth == '01']),
                             "February" = sum(sales[mth == '02']),
                             "March" = sum(sales[mth == '03']),
                             "April" = sum(sales[mth == '04']),
                             "May" = sum(sales[mth == '05']),
                             "June" = sum(sales[mth == '06']),
                             "July" = sum(sales[mth == '07']),
                             "August" = sum(sales[mth == '08']),
                             "September" = sum(sales[mth == '09']),
                             "October" = sum(sales[mth == '10']),
                             "November" = sum(sales[mth == '11']),
                             "December" = sum(sales[mth == '12'])
                             )

Top_Sales_Month <- Top_Sales_Month %>% 
  mutate(Highest_Total = pmax(`January`, `February`, `March`, `April`, `May`, `June`, `July`,
                              `August`, `September`, `October`, `November`, `December`),
         Top_Month = ifelse(`January` == Highest_Total, "January",
                               ifelse(`February` == Highest_Total, "February",
                                      ifelse(`March` == Highest_Total, "March",
                                          ifelse(`April` == Highest_Total, "April", 
                                                    ifelse(`May` == Highest_Total, "May", 
                                                           ifelse(`June` == Highest_Total, "June", 
                                                                  ifelse(`July` == Highest_Total, "July", 
                                                                         ifelse(`August` == Highest_Total, "August", 
                                                                                ifelse(`September` == Highest_Total, "September", 
                                                                                       ifelse(`October` == Highest_Total, "October", 
                                                                                              ifelse(`November` == Highest_Total, "November", "December"
                                                                                              ))))))))))))

Top_Sales_Month_Disp <- data.frame(Top_Sales_Month[, c(1, 14, 15)])
print(Top_Sales_Month_Disp)

#Assume the following quarters: Q1 (Jan, Feb, Mar), Q2 (April, May, June), 
#Q3 (July, Aug, Sep), and Q4 (Oct, Nov, Dec). Using data visualization to support your points, 
#describe the variation in sales by quarter.
Quarters_Details <- df %>% mutate(mth = format(mth, "%m")) %>% mutate(Quarter = ceiling(as.numeric(mth) / 3))

Quarters_Summary <- Quarters_Details %>% group_by(Quarter) %>% 
  summarize(Average_Sales = sum(sales) / n()) 

as.data.frame(Quarters_Summary)

plot1 <- ggplot(Quarters_Summary, aes(Quarter, Average_Sales)) + geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous("Average Sales", labels = comma) + theme_economist() + 
  theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) + 
  labs(title = "Averages Sales Amount per Quarter for the Last 12 Years")

print(plot1)

plot2 <- ggplot(Quarters_Details, aes(x = Quarter, y = sales, group = Quarter)) + geom_boxplot() + 
  scale_y_continuous("Sales", labels = comma) + theme_economist() + 
  theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) + 
  labs(title = "Sales Amount per Quarter for the Last 12 Years") + geom_jitter(width = 0.05, alpha = 0.3)

print(plot2)

#According to both plots, Q4 is the best month of the year for average sales, while Q1 is the least strong. 
#One could speculate that is due in part to the holiday season and the "retail hangover" that usually ensues.
#Plot2 also shows us that the range of sales transaction amounts is greatest in Q4, 
#with the highest gross amounts being in that quarter as well.  Interestingly enough, the median sale amount 
#also rises with each subsequent quarter as the year goes on, only returning to its lowest at the onset of a
#new year.  It is also interesting to note that in the 12 year history, not once has Q4 total sales at its
#worse been less than Q1's total sales at its best.
                