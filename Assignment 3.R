library(ggplot2)
library(ggthemes)
library(plotly)

df <- read.csv("2016 MLS Players.csv", stringsAsFactors = FALSE)

#Consolidating position values
df$Pos[df$Pos == "F"] <- "Forward" 
df$Pos[df$Pos == "F/M" | df$Pos == "F-M" | df$Pos == "M/F" | df$Pos == "M-F"] <- "Attacking Midfielder"
df$Pos[df$Pos == "M"] <- "Midfielder" 
df$Pos[df$Pos == "D-M" | df$Pos == "M-D"] <- "Defensive Midfielder"
df$Pos[df$Pos == "D"] <- "Defender"
df$Pos[df$Pos == "GK"] <- "Goalkeeper"
df$FullName <- paste(df$First, df$Last, sep = ' ')

##Converting goals and minutes to numeric
df$Minutes <- as.numeric(df$Minutes)
df$Goals <- as.numeric(df$Goals)

##Changing the NAs to a set value
est_mins = quantile(df$Minutes, .20, na.rm = T)
df$Minutes[which(is.na(df$Minutes))] = est_mins
est_goals = 0
df$Goals[which(is.na(df$Goals))] = est_goals

##Adding relevant calculated fields
df$MinPerGoal <- ifelse(df$Goals == 0, NA ,df$Minutes / df$Goals)
df$SalaryPerGoal <- ifelse(df$Goals == 0, NA ,df$Base.Salary / df$Goals)

focusPoints <- c("Bradley Wright", "Ignacio Piatti", "Kei Kamara", "Nicolas Lodeiro", "Didier Drogba", 
                 "Giovani Dos Santos", "Robbie Keane", "Clint Dempsey", "Jozy Altidor", "Frank Lampard", 
                 "David Villa", "Steven Gerrard", "Bradley Wright-Phillips", "Matt Hedges")

plot1 <- ggplot(df, aes(Goals, Base.Salary, color = Pos)) + geom_point() + 
            geom_smooth(method = lm, se = FALSE)
plot1 <- plot1 +  geom_label(aes(label = FullName), color = "gray20", 
            data = subset(df, FullName %in% focusPoints))
plot1 <- plot1 + theme_fivethirtyeight()

##plot1 <- ggplotly(plot1) 
