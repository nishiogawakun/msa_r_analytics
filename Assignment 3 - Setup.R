library(ggplot2)
library(ggthemes)
library(plotly)
library(scales)

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

##Plotting base salary and goals, all positions
focusPoints <- c("Bradley Wright", "Ignacio Piatti", "Kei Kamara", "Nicolas Lodeiro", "Didier Drogba", 
                 "Giovani Dos Santos", "Robbie Keane", "Clint Dempsey", "Jozy Altidor", "Frank Lampard", 
                 "David Villa", "Steven Gerrard", "Bradley Wright-Phillips", "Matt Hedges")
plotall <- ggplot(df, aes(Goals, Base.Salary, color = Pos)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) + scale_x_continuous(name = "Goals", limits = c(0, 28)) + 
  scale_y_continuous(name = "Base Salary", labels = comma)
plotall <- plotall +  geom_label(aes(label = FullName), color = "gray20", 
                                 data = subset(df, FullName %in% focusPoints))
plotall <- plotall + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  labs(title = "Scatterplot of Goals and Base Salary")
