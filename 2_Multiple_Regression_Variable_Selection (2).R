# Script file for Variable Selection

library(HH) 
data(longley) 
head(longley)
summary(longley)

splom(~ longley, pch=16, 
  pscales=2, 
  varname.cex=.8, 
  axis.text.cex=.5)

longley.lm <- lm( Employed ~ . , data=longley) 
summary(longley.lm)
vif(longley.lm)

# 3D plot of Employed ~ GNP + Year 
regr2.plot(longley[,"Year"],	xlab="Year",
  longley[,"GNP"], ylab="GNP", 
  longley[,"Employed"], zlab="Employed", 
  resid.plot="square",
  theta=-40, phi=25, r=sqrt(3), 
  box=is.R(),
  plot.back.planes=FALSE, 
  plot.base.plane=FALSE,
  main="Least squares with two highly 
  collinear X-variables")

# drop Population from complete model 
# in longley.lm 
longley2.lm <- lm( Employed ~
  GNP.deflator + GNP + Unemployed + 
    Armed.Forces + Year, data=longley)
summary(longley2.lm) 
vif(longley2.lm)

# drop GNP.deflator, highest p-value 
longley3.lm <- lm(Employed ~
  GNP + Unemployed + Armed.Forces + Year, 
  data=longley)
summary(longley3.lm) 
vif(longley3.lm)

# drop GNP, large VIF and largest p-value 
longley4.lm <- lm( Employed ~
  Unemployed + Armed.Forces + Year, 
  data=longley)
summary(longley4.lm) 
vif(longley4.lm)

# stepwise regression analysis of longley data 
longley.subsets <-
  leaps::regsubsets(Employed ~ GNP.deflator + 
                      GNP + Unemployed + 
                      Armed.Forces + 
                      Population + Year,
                    data=longley, nbest=2) 

longley.subsets.Summary <- 
  summaryHH(longley.subsets) 
longley.subsets.Summary

tmp <- (longley.subsets.Summary$cp <= 10) 
longley.subsets.Summary[tmp,]

plot(longley.subsets.Summary[tmp,], 
     statistic='cp', legend=FALSE)


longley.lm.7 <- lm.regsubsets(longley.subsets, 7)
# subset 7 has largest adjr2 
summary(longley.lm.7)

