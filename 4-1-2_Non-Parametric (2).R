# Lawsuit anlaysis
# Chi-square and non parametric 
#
library(HH)
require(vcd)## required package for new plots
lawsuit <- read.csv("lawsuit.csv", as.is=TRUE)
## if your working directory is not set use
## lawsuit <- read.csv("C:/Users/astrandb/Desktop/lawsuit.csv", header=TRUE)
## You will need to change the location of the file 
## to the saved location on your computer.

head(lawsuit)
dim(lawsuit)

#check for missing data
any(is.na(lawsuit$Type))     ## checks the Type variable 
any(is.na(lawsuit$Outcome))  ## checks the Outcome variable
any(is.na (lawsuit))         ## checks full dataset

table(lawsuit$Type)
table(lawsuit$Outcome)
table(lawsuit$Type, lawsuit$Outcome)
mosaicplot(table(lawsuit$Type)) # the start
mosaicplot(table(lawsuit$Type, lawsuit$Outcome)) # the real plot correct order
mosaicplot(table(lawsuit$Outcome, lawsuit$Type)) # incorrect order

mosaicplot(table(lawsuit$Type, lawsuit$Outcome),color= 4:5)

## run a chi-square test
chisq.test((table(lawsuit$Type, lawsuit$Outcome)))


## permutation test: via simulation estimated
## without making distributional assumptions:

## Randomly permute the labels (i.e. the
## Outcome, althought it really does not matter which we choose)
## thus actually breaking the association between Type and Outcome.
lawsuit$SimOutcome <- sample(lawsuit$Outcome)
names(lawsuit) # show column names - useful for debugging
head(lawsuit)

table(lawsuit$Type, lawsuit$SimOutcome) 
mosaicplot(table(lawsuit$Type, lawsuit$SimOutcome),color= 4:5)
## since this is a simulation your result may not match
## Note that the result with table(lawsuit$Type, lawsuit$SimOutcome) are 
## different from table(lawsuit$Type, lawsuit$Outcome)

# Now repeat the simulation to test the null
# distribution of our test statistic!

B <- 10 # repeat 10 times
# set up
ans <- rep(NA, B) # stores the answers of the 10 games - NA creating an empty
for (i in 1:B) {
    lawsuit$SimOutcome <- sample(lawsuit$Outcome)
  ans[i] <- table(lawsuit$Type, lawsuit$SimOutcome)[1,1] 
  # extract the number in the first row and first col to ans
}
ans
# returns number of new moms fired each time

# to run 10000 times and estimate p-value
set.seed(1) # do not put this inside the loop - bad idea!
## setting your seed is optional but may be needed if you will be required 
## to reproduce your results

B <- 10000
ans <- rep(NA, B) 
for (i in 1:B) {
  lawsuit$SimOutcome <- sample(lawsuit$Outcome)
  ans[i] <- table(lawsuit$Type, lawsuit$SimOutcome)[1,1] 
  # extract the number in the first row and first col to ans
}

head(ans) # result of the first 6 games
table(ans)

sum(ans==8) # interesting but we can do better
sum(ans>= 8) # very useful

sum(ans>=8)/B # our estimate of the p-value

## Compared to chi square p-value
chisq.test((table(lawsuit$Type, lawsuit$Outcome)))

