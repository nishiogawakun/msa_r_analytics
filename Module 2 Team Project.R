library(HH)
data(uscrime)
head(uscrime, 50)
summary(uscrime)

uscrime$S <- as.factor(uscrime$S)
summary(uscrime$S)

# the State variable should not be included in the regression analysis
# since this is a label (not a predictor)
uscrime.var <- uscrime[, 1:14] ## removes column 15 the state variable
head(uscrime.var)

# for your regression model remember to have data=uscrime.var
# Changing the southern field to factor with labels
uscrime.var$S <- factor(uscrime.var$S, levels = c(0, 1),
                             labels = c("Non-Southern", "Southern"))

# Creating a splom for reference 
splom(~uscrime.var[,c(1:14)], pch = 16, 
      cex=.35, main = "US Crime Rates", 
      superpanel = panel.pairs, axis.text.cex = 0, 
      pscales = 0, panel.cex = 0)

# Creating a splom for further detail on southern states
tpg <- trellis.par.get("superpose.symbol") 
tpg <- lapply(tpg, function(x) x[1:2]) 
splom(~uscrime.var[c(1, 2, 4:14)],
      cex = .65, 
      panel = panel.superpose, 
      key = list(space = "right",
               text = list(levels(uscrime.var$S)), 
               points = tpg,
               border = 1, 
               title = "Southern", 
               cex = 1),
      group = uscrime.var$S, 
      data = uscrime, 
      main = "Crime Data Delineated by Southern States",
      superpanel = panel.pairs, axis.text.cex = 0, 
      subpanel.scales = list(cex = .8), pscales = 0, 
      panel.cex = 1.5,
      pch = c(15, 16))

# Full regression model
# Model overestimates more often than underestimates
uscrime.var.lm <- lm(R ~ Age + S + Ed + Ex0 + Ex1 + LF + M + N + NW + U1 + U2 + W + X,
                    data = uscrime.var)
summary(uscrime.var.lm)

# Testing full regression model for multicollinearity issues
vif(uscrime.var.lm)

# 3D plot of of Ex0, Ex1, and R to further reinforce that collinarity problem exists
regr2.plot(uscrime.var[,"Ex0"],	xlab="Ex0",
           uscrime.var[,"Ex1"], ylab="Ex1", 
           uscrime.var[,"R"], zlab="R", 
           resid.plot="square",
           theta=-40, phi=25, r=sqrt(3), 
           box=is.R(),
           plot.back.planes=FALSE, 
           plot.base.plane=FALSE,
           main="Least squares with two highly 
  collinear X-variables")

# Removed Ex1 and W  due to multicollinearity issue, high VIF
uscrime.var.lm2 <- lm(R ~ Age + S + Ed + Ex0 + LF + M + N + NW + U1 + U2 + X,
                     data = uscrime.var)
summary(uscrime.var.lm2)

# Testing full regression model for multicollinearity issues
vif(uscrime.var.lm2)

# Removed LF and N due to very high P values, deemed not significant
uscrime.var.lm3 <- lm(R ~ Age + S + Ed + Ex0 + M + NW + U1 + U2 + X,
                      data = uscrime.var)
summary(uscrime.var.lm3)

# Removed S and NW due to very high P values, deemed not significant
uscrime.var.lm4 <- lm(R ~ Age + Ed + Ex0 + M + U1 + U2 + X,
                      data = uscrime.var)
summary(uscrime.var.lm4)
vif(uscrime.var.lm4)

# Removed U1 and  due to high P values, deemed not significant. U1 was considered, but 
# ultimately removed due to higher VIF result most likely connected to U2
# !!This is the selected model I chose to move forward analyzing!!
uscrime.var.lm5 <- lm(R ~ Age + Ed + Ex0 + U2 + X,
                      data = uscrime.var)
summary(uscrime.var.lm5)
vif(uscrime.var.lm5)

# Automate stepwise regression variable selection
uscrime.var.subsets <-
  leaps::regsubsets(R ~ Age + S + Ed + Ex0 + LF + M + N + NW + U1 + U2 + X,
                    data = uscrime.var, nbest=2) 

uscrime.var.subsets.Summary <- 
  summaryHH(uscrime.var.subsets) 
uscrime.var.subsets.Summary

# Filtering for results with CP value less than or equal to 10
tmp <- (uscrime.var.subsets.Summary$cp <= 10) 
uscrime.var.subsets.Summary[tmp,]

# Plotting those results (from above)
plot(uscrime.var.subsets.Summary[tmp,], 
     statistic='cp', legend=FALSE)

# Looking at a summary/vif for model 9 of the automated stepwise selection
# This model had the lowest CP value (and matches the manual result we chose)
uscrime.var.lm6 <- lm.regsubsets(uscrime.var.subsets, 9)
summary(uscrime.var.lm6)
vif(uscrime.var.lm6)

# Looking at a summary/vif for model 13 of the automated stepwise selection
# This value had the highest r2 value, but was not selected due to including
# statistically insignificant variables
uscrime.var.lm7 <- lm.regsubsets(uscrime.var.subsets, 13)
summary(uscrime.var.lm7)
vif(uscrime.var.lm7)

# Partial Residual Plots for full model 
residual.plots.lattice(
  uscrime.var.lm, par.strip.text=list(cex=1.1))

# Residual plot lattice for final model
# Linear relationship, no curvature/bunching/funnel (= constant variance),
# P-values are most likely represented fairly in 3rd row
# Row 4 does not show any non-zero slope
residual.plots.lattice(
  uscrime.var.lm5, par.strip.text=list(cex=1.1))

# Residual Plots for the selected reduced model
# Random scatter, residuals are close to the normal q-q line (approx. normal), 
# R2 seems represnted fairly in square 4 (since it's lower, not flat on 2nd rectangle)
lmplot(uscrime.var.lm5)

# Diagnostics for reduced model 
uscrime.var.lm5.case <- case(uscrime.var.lm5) 
uscrime.var.lm5.case

uscrime.var.lm5.trellis <-
  plot(uscrime.var.lm5.case, uscrime.var.lm5, 
       par.strip.text=list(cex=1.2), 
       layout=c(3,4), main.cex=1.6)
uscrime.var.lm5.trellis

summary(uscrime.var.lm5)

# Prediction interval
pi.fit <- predict(uscrime.var.lm5, 
                  newdata=data.frame(Age = 136, Ed =109, Ex0 = 83, U2 = 35, X = 176),
                  se.fit=TRUE, 
                  interval = "prediction")
pi.fit

# Confidence interval
ci.fit <- predict(uscrime.var.lm5, 
                  newdata=data.frame(Age = 136, Ed =109, Ex0 = 83, U2 = 35, X = 176),
                  se.fit=TRUE, 
                  interval = "confidence")
ci.fit


