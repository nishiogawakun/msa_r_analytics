library(ggplot2)
library(ggthemes)
library(plotly)
library(scales)
library(lattice)
library(dplyr)
library(magrittr)

##Reading the CSV
df <- read.csv("2016 MLS Players.csv", stringsAsFactors = FALSE)

##Consolidating position values
df$Pos[df$Pos == "F"] <- "Forward" 
df$Pos[df$Pos == "F/M" | df$Pos == "F-M" | df$Pos == "M/F" | df$Pos == "M-F"] <- "Attacking Midfielder"
df$Pos[df$Pos == "M"] <- "Midfielder" 
df$Pos[df$Pos == "D-M" | df$Pos == "M-D"] <- "Defensive Midfielder"
df$Pos[df$Pos == "D"] <- "Defender"
df$Pos[df$Pos == "GK"] <- "Goalkeeper"

##Adding a separate Full Name field
df$FullName <- paste(df$First, df$Last, sep = ' ')

##Converting goals and minutes to numeric
df$Minutes <- as.numeric(df$Minutes)
df$Goals <- as.numeric(df$Goals)

##Removing NAs
df <- df[complete.cases(df),]

##Linear modeling
model <- lm(Base.Salary ~ Minutes, data = df)
summary(model)

## Generate a vector of predicted values and store as lm_predict2
lm_predict2 <- predict(model)

## Append predicted values and residuals to forwards data
Base_Salary_Predict2 <- data.frame(df, lm_predict2, model$residuals)

## Top 5 overperforming by minutes played
Top_5_overperforming2 <- head(Base_Salary_Predict2 %>% 
                                arrange(Base_Salary_Predict2$model.residuals), 5)

## Top 5 underperformaing by minutes played
Top_5_underperforming2 <- head(Base_Salary_Predict2 %>% 
                                 arrange(desc(Base_Salary_Predict2$model.residuals)), 5)

## Create a table to display Total Amount of Salary money allocated by position, including a column for percent
PieTable <- df %>% group_by(Pos) %>% summarise("Total Amount" = sum(Base.Salary)) %>% 
  mutate(per = `Total Amount`/sum(`Total Amount`))

## get the color brewer pallete
require("RColorBrewer")

## save colors to vector
pieColors <- brewer.pal(6,"Spectral")

## paste slice labels to vector
pieLabels <- paste(percent(round(PieTable$per,2)), sep = ":")

## paste legend labels to vector
PieLegend <- paste(PieTable$Pos)

## adjust margins to leave room for Title and margin
par(mar = c(0,0,4,8))

## create pie chart using previous defined variables
pie(PieTable$per, pieLabels, col = pieColors, main = "Allocations of Total Money by Position")

## create legend
legend("topright", inset = c(-.2,0), PieLegend, fill = pieColors)

PieTable6 <- df %>% group_by(Club) %>% 
  summarise("Total Spent" = sum(Base.Salary), 
            "Forward" = sum(Base.Salary[Pos == "Forward"]),
            "Attacking Midfield" = sum(Base.Salary[Pos == "Attacking Midfield"]),
            "Midfielder"  = sum(Base.Salary[Pos == "Midfielder"]),
            "Defensive Midfielder" = sum(Base.Salary[Pos == "Defensive Midfielder"]),
            "Defender" = sum(Base.Salary[Pos == "Defender"]),
            "Goalkeeper" = sum(Base.Salary[Pos == "Goalkeeper"]))
View(PieTable6)

## Create table for the Total Allocation of Money by Position for Each Team
PieTable2 <- df %>% group_by(Pos) %>%
  summarise("CHI" = sum(Base.Salary[Club == "CHI"]),
            "CLB" = sum(Base.Salary[Club == "CLB"]),
            "COL" = sum(Base.Salary[Club == "COL"]),
            "DAL" = sum(Base.Salary[Club == "DAL"]),
            "DC" = sum(Base.Salary[Club == "DC"]),
            "HOU" = sum(Base.Salary[Club == "HOU"]),
            "KC" = sum(Base.Salary[Club == "KC"]),
            "LA" = sum(Base.Salary[Club == "LA"]),
            "MTL" = sum(Base.Salary[Club == "MTL"]),
            "NE" = sum(Base.Salary[Club == "NE"]),
            "NYCFC" = sum(Base.Salary[Club == "NYCFC"]),
            "NYRB" = sum(Base.Salary[Club == "NYRB"]),
            "ORL" = sum(Base.Salary[Club == "ORL"]),
            "POR" = sum(Base.Salary[Club == "POR"]),
            "SEA" = sum(Base.Salary[Club == "SEA"]),
            "SJ" = sum(Base.Salary[Club == "SJ"]),
            "RSL" = sum(Base.Salary[Club == "RSL"]),
            "TOR" = sum(Base.Salary[Club == "TOR"]),
            "VAN" = sum(Base.Salary[Club == "VAN"])) 
View(PieTable2)

## Add a collumn to the end of the table that includes the Average Team Allocation of Money by Position
PieTable3 <- PieTable2 %>%  mutate("Avg per Team" = rowMeans(PieTable2[,-1]))
View(PieTable3)

## Add a collumn to the end of the table that shows the Avgerage Team Percent Allocation of Money by Position
PieTable4 <- PieTable3 %>% mutate("Avg percent per Team" = `Avg per Team`/sum(`Avg per Team`))
View(PieTable4)

## get the color brewer pallete
require("RColorBrewer")

## save colors to vector
pieColors <- brewer.pal(6,"Spectral")

## paste slice labels to vector
pieLabels <- paste(percent(round(PieTable4$`Avg percent per Team`,3)))

## paste legend labels to vector
PieLegend <- paste(PieTable4$Pos)

## adjust margins to leave room for Title and margin
par(mar = c(0,0,4,4))

## create pie chart using previous defined variables
pie(PieTable4$`Avg percent per Team`, pieLabels, col = pieColors, main = "Average Team Allocations of Money by Position")

## create legend
legend("topright", inset = c(-.11,0), PieLegend, fill = pieColors)

##Scatterplot plotting minutes played and base salary, by positions
plotall2 <- ggplot(df, aes(Minutes, Base.Salary, color = Pos)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) + scale_x_continuous(name = "Minutes Played", labels = comma) + 
  scale_y_continuous(name = "Base Salary ($)", labels = comma)
plotall2 <- plotall2 +  theme(plot.title = element_text(hjust = 0.5, face = "bold.italic"),
                              axis.title.x = element_text(face = "bold"),
                              axis.title.y = element_text(face = "bold"),
                              plot.caption = element_text(hjust = 0, face = "italic"),
                              legend.position = "bottom") + 
  theme(axis.title = element_text()) +
  labs(title = "Scatterplot of Minutes Played and Base Salary") +
  facet_wrap(~ Pos, scales = "free")

plot(plotall2)

##Scatterplot plotting minutes played and goals, all positions
focusPoints <- c("Keegan Rosenberry", "Steven Gerrard", "David Villa")

plotall3 <- ggplot(df, aes(Minutes, Goals, color = Pos)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) + scale_x_continuous(name = "Minutes Played", labels = comma, limits = c(0, 3700)) + 
  scale_y_continuous(name = "Goals", labels = comma)
plotall3 <- plotall3 +  geom_label(aes(label = FullName), color = "gray20", 
                                 data = subset(df, FullName %in% focusPoints))
plotall3 <- plotall3 +  theme(plot.title = element_text(hjust = 0.5, face = "bold.italic"),
                              axis.title.x = element_text(face = "bold"),
                              axis.title.y = element_text(face = "bold"),
                              plot.caption = element_text(hjust = 0, face = "italic")) + 
  theme(axis.title = element_text()) +
  labs(title = "Scatterplot of Minutes Played and Goals Scored")

plot(plotall3)

##Scatterplot plotting minutes played and base salary, all positions
plotall4 <- ggplot(df, aes(Minutes, Base.Salary)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) + scale_x_continuous(name = "Minutes Played", labels = comma, limits = c(0, 3500)) + 
  scale_y_continuous(name = "Base Salary ($)", labels = comma)
plotall4 <- plotall4 +  theme(plot.title = element_text(hjust = 0.5, face = "bold.italic"),
                              axis.title.x = element_text(face = "bold"),
                              axis.title.y = element_text(face = "bold"),
                              plot.caption = element_text(hjust = 0, face = "italic")) + 
  theme(axis.title = element_text()) +
  labs(title = "Scatterplot of Minutes Played and Base Salary")

plot(plotall4)

## Residual diagnostics for minute played and salary.  

plot(xyplot(resid(model) ~ fitted(model),
       xlab = "Fitted Values",
       ylab = "Residuals",
       main = "Residual Diagnostic Plot",
       panel = function(x, y, ...)
       {
         panel.grid(h = -1, v = -1)
         panel.abline(h = 0)
         panel.xyplot(x, y, ...)
       }
))

plot(qqmath( ~ resid(model),
        xlab = "Theoretical Quantiles",
        ylab = "Residuals"
))

##Create a new data frame that includes only forwards

forwards <- df %>% filter(df$Pos == "Forward")
attach(forwards)

## Create a scatterplot of Base Salary vs. Goals Scored for forwards

plot_1 <- ggplot(forwards, aes(y = Base.Salary, x = Goals)) + 
  geom_point() + 
  ggtitle("Base Salary vs. Goals Scored for Forwards") + 
  labs(caption ="Note: The number of minutes played for a specific player are depicted for players who scored at least 10 goals.") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold.italic"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, face = "italic", color = "red")) + 
  scale_y_continuous(labels = scales::dollar) + 
  ylab("Base Salary") + 
  xlab("Goals Scored") + 
  geom_smooth(method = "lm", se = F) + 
  geom_text(aes(label = ifelse(Goals > 9,   
                               as.character(Minutes), " ")), hjust = 0, vjust = 0, size = 3, col = "red")

plot(plot_1)

## Develop a linear regression model for Base Salary (y) vs. Goals Scored (x)
## and store as lm_model

lm_model <- lm(Base.Salary ~ Goals)
summary(lm_model)

## Generate a vector of predicted values and store as lm_predict
lm_predict <- predict(lm_model)

## Append predicted values and residuals to forwards data
Base_Salary_Predict <- data.frame(forwards, lm_predict, lm_model$residuals)

## Top 5 overperforming forwards
Top_5_overperforming <- head(Base_Salary_Predict %>% arrange(Base_Salary_Predict$lm_model.residuals), 5)
Top_5_overperforming

## Top 5 underperformaing forwards
Top_5_underperforming <- head(Base_Salary_Predict %>% arrange(desc(Base_Salary_Predict$lm_model.residuals)), 5)
Top_5_underperforming

detach(forwards)