##Scatterplot plotting base salary and goals, all positions
plotall2 <- ggplot(df, aes(Minutes, Base.Salary, color = Pos)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) + scale_x_continuous(name = "Minutes Played", labels = comma) + 
  scale_y_continuous(name = "Base Salary ($)", labels = comma)
plotall2 <- plotall2 + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  labs(title = "Scatterplot of Minutes Played and Base Salary")