###########################################################
### code from 
### DataDR and Trelliscope example with the adult 
### dataset (census data)
### http://deltarho.org/
###########################################################

install.packages(c("datadr", "trelliscope", 
                   "lattice"))

library(datadr)
library(trelliscope)
library(lattice)

# packageDescription("datadr")
# help(package = "datadr")

# look at the dataset
str(adult)

head(adult)
tail(adult)
summary(adult)


# express data as a local distributed data frame

adultDdf <- ddf(adult, update=TRUE)
adultDdf

# look at data summary statistics

summary(adultDdf)

# divide the ddf by the variable "education"

byEducation <- divide(adultDdf, by = "education",
                      update=TRUE)

# look at division keys

getKeys(byEducation)

# look at subsets of the ddf

byEducation[["education=Bachelors"]][[1]]

head(byEducation[["education=Bachelors"]][[2]])

# compute mean and standard dev of hours per week 
# for each subset and recombine into one table

edMeanAndVar <- recombine(byEducation,
   apply = function(x) {
      list(
         mean=mean(x$hoursperweek, na.rm=TRUE), 
         stdev=sqrt(var(x$hoursperweek, 
                        na.rm=TRUE)))},
   combine = combRbind())

# look at results 

edMeanAndVar
   
# divide by work class

byWorkClass <- divide(adultDdf, by="workclass")

getKeys(byWorkClass)

# panel function: education vs hours per week, 
# conditioned on income category

pf1 <- function(x)
   xyplot(hoursperweek ~ educationnum | income, 
      data = x,
      xlab="Years of Education",
      ylab="Hours per Week")
   
# test it on one data division

pf1(byWorkClass[["workclass=Federal-gov"]][[2]])

# cognostics function

cf <- function(x) {
   list(
      fracAbove50K = cog(mean(x$incomebin), 
        desc = "fraction above $50K income"),
      minEducation = cog(min(x$educationnum), 
        desc = "min number of education years"),
      meanEducation = cog(mean(x$educationnum), 
        desc = "mean number of education years"),
      maxEducation = cog(max(x$educationnum), 
        desc = "max number of education years"),
      minnHoursPerWeek = cog(min(x$hoursperweek), 
        desc="min hours per week worked"),
      meanHoursPerWeek = cog(mean(x$hoursperweek), 
        desc="mean hours per week worked"),
      maxHoursPerWeek = cog(max(x$hoursperweek), 
        desc="max hours per week worked"))}

cf(byWorkClass[["workclass=Federal-gov"]][[2]])
 
# create visualization database 
# this will take a moment

vdbConn("vdb_census", autoYes = TRUE)
   
# create display with this panel and cognostics
# functions

makeDisplay(byWorkClass,
   name = "hours_and_education_by_workclass",
   desc = "hours and education by workclass",
   panelFn = pf1, cogFn = cf,
   width = 400, height = 400)

# view the display
view()


# add a graph 

# panel function: age vs hours per week, 
# conditioned on income category

pf2 <- function(x)
  xyplot(hoursperweek ~ age | income, 
         data = x,
         xlab="Years of Age",
         ylab="Hours per Week")

# test it on one data division

pf2(byWorkClass[["workclass=Federal-gov"]][[2]])


# a second type of plot on the same data
makeDisplay(byWorkClass,
 name = "hours_and_age_scatterplot_by_workclass",
 desc = "hours and age scatterplot by workclass",
            panelFn = pf2, cogFn = cf,
            width = 400, height = 400)

# launch viewer
view()
