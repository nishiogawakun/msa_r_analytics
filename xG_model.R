#Install and access dplyr package for data manipulation
install.packages("dplyr") 
library (dplyr)


shots <- read.csv("MLS_2015_Shots_Train.csv") #import training data


##Run logistic regression for all variables in current shots data frame
xG_model <- glm(
  goal ~  distance_to_goal + shot_angle + big_chance + header+ assisted
  , family = binomial(logit)
  , data = shots)
summary(xG_model) #Regression summary

##Re-run logistic regression with significant indepedent variables
xG_model_final <- glm(
  goal ~  distance_to_goal + shot_angle + big_chance + header
  , family = binomial(logit)
  , data = shots)
summary(xG_model_final) #Regression summary

##Store predicted values as expected goals (XG) 
xG <- predict(xG_model_final,shots,type = "response")

sum(xG) #overall expected goals sum
sum(shots$goal) # overall goals sum
shots <- data.frame(shots,xG) #append expected goals to shots data
head(shots) #look at the first 6 rows 

##Create table that groups expected goals, goals, and shots by team
Total_shots <- read.csv("MLS_2014_Shots_Total.csv") #Import entire 2014 season of shots
str(Total_shots)
##Total_shots$shot_angle <-  as.numeric(levels(Total_shots$shot_angle))[Total_shots$shot_angle]
xG <- predict(xG_model_final,Total_shots,type = "response") #Find xG values
Total_shots <- data.frame(Total_shots,xG) #Merge Total_shots with xG
#Dplyr routine for group_by(team)
dfs <- Total_shots %>% 
                group_by(teamname) %>% 
                summarise(Expected_goals =sum(xG),Actual_goals=sum(goal),
                          Total_shots=n())
dfs$diff <- dfs$Actual_goals - dfs$Expected_goals

