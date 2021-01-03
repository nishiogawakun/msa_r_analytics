##Scatterplot plotting base salary and goals, all positions
focusPoints <- c("Bradley Wright", "Ignacio Piatti", "Kei Kamara", "Nicolas Lodeiro", "Didier Drogba", 
                 "Giovani Dos Santos", "Robbie Keane", "Clint Dempsey", "Jozy Altidor", "Frank Lampard", 
                 "David Villa", "Steven Gerrard", "Bradley Wright-Phillips", "Matt Hedges")
plotall <- ggplot(df, aes(Goals, Base.Salary, color = Pos)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) + scale_x_continuous(name = "Goals", limits = c(0, 28)) + 
  scale_y_continuous(name = "Base Salary ($)", labels = comma)
plotall <- plotall +  geom_label(aes(label = FullName), color = "gray20", 
                                 data = subset(df, FullName %in% focusPoints))
plotall <- plotall + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  labs(title = "Scatterplot of Goals and Base Salary")