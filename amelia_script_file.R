install.packages(c("Amelia", "HH","Zelig"))

library(Amelia)
library(HH)
library(Zelig)

packageDescription("Amelia")
help(package = "Amelia")


data(freetrade)
# data from Milner and Kubota (2005)

head(freetrade)
tail(freetrade)

summary(freetrade)


freetrade$signed <- as.factor(freetrade$signed)
summary(freetrade$signed)

any(is.na(freetrade)) 
# checks the full dataset for missing data

sum(is.na (freetrade))  
# total number of missing variables


# Data Displays
missmap(freetrade) # first Amelia function

splom(freetrade) # from HH package

# consider year, country, tariff, polity, pop, gdp.pc, 
splom(freetrade[,c(1,3:6)], cex=.5,
      main="Free Trade by Signed")


# Regression
freetrade.lm <- lm(tariff ~ polity + pop 
                   + gdp.pc + year + country,
                   data = freetrade)

summary(freetrade.lm)
# 60 observations deleted due to missingness
# These observations are partially observed, 
# containg some valuable information 

# Imputating Data with Amelia

# second Amelia function
a.out <- amelia(freetrade, m = 5, ts = "year", 
                 cs = "country", noms = "signed", 
                 ords = "polity")
# IDs polity as a ordinal and signed as nominal

summary(a.out)

names(a.out)
names(a.out$imputations)
names(a.out$imputations$imp1)
head(a.out$imputations$imp1)

save(a.out, file = "imputations.RData")

getwd()
#third Amelia function
write.amelia(obj=a.out, file.stem = "outdata")


# time series plots on imputated data
# fourth Amelia function
tscsPlot(a.out, cs = "Korea", main = "Korea",
         var = "tariff", ylim = c(-30, 70))

freetrade[freetrade$country == "Korea", 
          c("year","country","tariff")]



# Regression with Impututed data from Amelia

# need Zelig package

freetrade.lm.imp <- zls$new()

freetrade.lm.imp$zelig(tariff ~ polity + pop 
                       + gdp.pc + year 
                       + country, data = a.out)


freetrade.lm.imp 
# regression model using Amelia
summary(freetrade.lm) 
# orginal regression model