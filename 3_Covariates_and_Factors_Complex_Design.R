# Script for Covarites and Fators
# Complex Design

# testscore data:
# P. O. Johnson and F. Tsao, 1945
# R. L. Anderson and T. A. Bancroft, 1952

# Note that: the models and model numbers 
# in this script may be different than models
# and model numbers discussed in the textbook

library(HH) 
data(testscore) 
head(testscore)
summary(testscore)

# interaction plot
interaction2wt(data=testscore, final ~ 
                 sex + grade + standing + order,
               xlim=c(.5, 3.5),
               between=list(x=.5, y=.5))

# order the levels of the factors
# to improve the simplicity of the graphs 
testscore$standing	<- ordered(
  testscore$standing, 
  levels=c("poor", "average", "good")) 

testscore$order	<- ordered(
  testscore$order,
  levels=c("low", "medium", "high"))

# re run interaction plot with correct ordering
interaction2wt(data=testscore, final ~ 
                 sex + grade + standing + order,
               xlim=c(.5, 3.5),
               between=list(x=.5, y=.5))

# Scatter Plot Matrix 
splom( ~ testscore, pch=16,
       main="Scatter Plot of Test Scores", 
       axis.text.cex=.6, xlab=NULL)

# shorter level names for splom 
levels(testscore$standing)[2] <- "avg" 
levels(testscore$order)[2] <- "med"

# re run Scatter Plot Matrix 
splom( ~ testscore, pch=16,
       main="Scatter Plot of Test Scores", 
       axis.text.cex=.6, xlab=NULL)

# restore longer level names 
levels(testscore$standing)[2] <- "average" 
levels(testscore$order)[2] <- "medium"

# Factors only
# three-way interactions
summary(aov(final ~ 
            (sex + grade + standing + order)^3,
              data=testscore))

# four-way interactions
summary(aov(final ~ 
            (sex + grade + standing + order)^4,
            data=testscore))

# two-way interactions
testscore1.aov <- aov(final ~ 
            (sex + grade + standing + order)^2,
            data=testscore) 

summary(testscore1.aov)

# Continuous variables only
testscore2.aov <- aov(final ~ 
                        initial + mental.age,
                      data=testscore)
summary(testscore2.aov)
coef(testscore2.aov)

testscore2.lm <- lm(final ~ 
                        initial + mental.age,
                      data=testscore)
summary(testscore2.lm)

# Continuous variables first, then factors with 
# two-way interactions 
testscore3.aov <- aov(final ~ 
            initial + mental.age +
            (sex + grade + standing + order)^2,
            data=testscore)

summary(testscore3.aov)

# Factors first, then continuous variables 
# with two-way interactions
testscore4s.aov <-
  aov(terms(final ~
              sex + grade + standing + order +
              sex:grade + sex:standing + 
              sex:order + grade:standing + 
              grade:order + standing:order + 
              initial + mental.age,
            keep.order=TRUE), data=testscore)

summary(testscore4s.aov)

# Comparisons
# Factor vs both 
anova(testscore1.aov, testscore4s.aov)

# Continuous vs both 
anova(testscore2.aov, testscore4s.aov)

# Refining model, delete most interactions, 
# move sex to last factor
testscore5.aov <- aov(final ~ initial + 
                         mental.age + grade + 
                         standing + order + 
                         sex + sex:grade + 
                         grade:standing, 
                       data=testscore)

summary(testscore5.aov)

# Remove all interactions 
testscore6.aov <- aov(final ~ initial + 
                        mental.age + grade +
                        standing + order +
                        sex,
                      data=testscore)

summary(testscore6.aov)

# Using model to predict
newdata <- cbind(initial=
                   mean(testscore$initial),
                 mental.age=
                   mean(testscore$mental.age),
                 testscore[c("grade", "standing",
                             "order","sex")])

newdata

final.pred <- predict(testscore6.aov, 
                      newdata=newdata) 

final.pred.table <- tapply(final.pred, 
                           newdata[,3:6], c) 

final.pred.table
