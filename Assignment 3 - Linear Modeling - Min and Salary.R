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


## Residual diagnostics for minute played and salary.  
## According to the web, these Characteristics of a well behaved residual vs fitted plot:
#### (1)The residuals spread randomly around the 0 line indicating that the relationship is linear.
#### (2)The residuals form an approximate horizontal band around the 0 line indicating homogeneity of error variance.
#### (3)No one residual is visibly away from the random pattern of the residuals indicating that there are no outliers.
## None of those really apply here so that lead us to believe that this linear model 
## isn't particularly good for predicting anything...IS this the correct interpretation?


xyplot(resid(model) ~ fitted(model),
       xlab = "Fitted Values",
       ylab = "Residuals",
       main = "Residual Diagnostic Plot",
       panel = function(x, y, ...)
       {
         panel.grid(h = -1, v = -1)
         panel.abline(h = 0)
         panel.xyplot(x, y, ...)
       }
)

## More residual diagnostics for minute played and salary
## According to the webs again, We would hope that this plot showed something approaching
## a straight line to support the model assumption about the distribution of the residuals.
## It doesn't so this linear model isn't very appropriate.
qqmath( ~ resid(model),
        xlab = "Theoretical Quantiles",
        ylab = "Residuals"
)

## Multiple models
mtcars2 = melt(mtcars, id.vars='mpg')
ggplot(mtcars2) +
  geom_jitter(aes(value,mpg, colour=variable),) + geom_smooth(aes(value,mpg, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x") +
  labs(x = "Percentage cover (%)", y = "Number of individuals (N)")