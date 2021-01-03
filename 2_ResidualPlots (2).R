# Script file for Residual Plots

# The code below is a continuation from 
# Multiple Regression Variable Selection
# This script file also uses the Longley dataset

library(HH) 
data(longley) 
head(longley)

# full model, includes all possible predictors
longley.lm <- lm( Employed ~ . , data=longley) 

summary(longley.lm)
vif(longley.lm)

# model from manual process - reduced model
longley4.lm <- lm( Employed ~
                     Unemployed + 
                     Armed.Forces + Year, 
                   data=longley)
summary(longley4.lm)
vif(longley4.lm)

# Partial Residual Plots for full model 
residual.plots.lattice(
  longley.lm, par.strip.text=list(cex=1.1))

# Residual Plots for reduced model
lmplot(longley4.lm)

# Diagnostics for reduced model 
longley4.lm.case <- case(longley4.lm) 
longley4.lm.case

longley4.lm.trellis <-
  plot(longley4.lm.case, longley4.lm, 
       par.strip.text=list(cex=1.2), 
       layout=c(3,3), main.cex=1.6)
longley4.lm.trellis

longley[c(3,4,6,7)]