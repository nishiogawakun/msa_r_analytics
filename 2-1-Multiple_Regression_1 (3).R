# Script file for Regression Part 1

library(HH) 
data(houseprice) 
head(houseprice)
summary(houseprice)

#scatter plot of price and sqft
plot(houseprice[,"sqft"], houseprice[,"price"], 
  ylab="", xlab="", 
  xlim=c(1000,3500),ylim=c(500,2100))
mtext("sqft", line=3, cex=1.2, side=1) 
mtext("price", line=3, cex=1.2, side=2)

# draw the regression line

regr1.plot(houseprice[,"sqft"], 
           houseprice[,"price"], 
  xlim=c(1000,3500),ylim=c(500,2100), main="",  
  ylab="", xlab="")
mtext("sqft", line=3, cex=1.2, side=1) 
mtext("price", line=3, cex=1.2, side=2)

# draw the residuals

regr1.plot(houseprice[,"sqft"], 
           houseprice[,"price"], 
  ylab="", xlab="",
  resid.plot="line", main="", 
  xlim=c(1000,3500), ylim=c(500,2100))
mtext("sqft", line=3, cex=1.2, side=1) 
mtext("price", line=3, cex=1.2, side=2)

# square the residuals 
regr1.plot(houseprice[,"sqft"], 
           houseprice[,"price"],
            xaxt="n", yaxt="n",
            ylab="", xlab="", 
            resid.plot="square", main="", 
            xlim=c(1000,3500), ylim=c(500,2100))
axis(1) 
axis(2)
mtext("price", line=3, cex=1.2, side=2) 
mtext("sqft", line=3, cex=1.2, side=1)

houseprice.lm.simple <- lm(price ~ sqft, 
                           data=houseprice) 
summary(houseprice.lm.simple) 














# Multiple Regression

# library(HH) 
# data(houseprice)

head(houseprice)
summary(houseprice)

houseprice$customf <- factor(houseprice$custom,
                             levels=c(0,1), 
                             labels=c("regular",
                                      "custom"))
houseprice$cornerf <- factor(houseprice$corner,
                             levels=c(0,1), 
                             labels=c("middle",
                                      "corner"))

head(houseprice)
summary(houseprice)
# Splom all predictors
splom(~houseprice[,c(1,2,5,6,7)], pch=16, 
      cex=.35, main="houseprice", 
      superpanel=panel.pairs, axis.text.cex=.7, 
      pscales=3, panel.cex=1)

# Splom Houseprice by Custom
tpg <- trellis.par.get("superpose.symbol") 
tpg <- lapply(tpg, function(x) x[1:2]) 
splom(~houseprice[c(1,2,5)],
  cex=.65, 
  panel=panel.superpose, 
  key=list(space="right",
    text=list(levels(houseprice$customf)), 
    points=tpg,
    border=1, 
    title="custom", 
    cex=1),
  group=houseprice$customf, 
  data=houseprice, 
  main="houseprice by custom",
  superpanel=panel.pairs, 
  subpanel.scales=list(cex=.8), pscales=4, 
  panel.cex=1.5,
  pch=c(15,16))

# Splom Houseprice by Custom |Corner
tpg <- trellis.par.get("superpose.symbol") 
tpg <- lapply(tpg, function(x) x[1:2]) 
splom(~houseprice[c(1,2,5)] | cornerf,
  panel=panel.superpose, 
  layout=c(2,1), 
  key=list(space="top",
    text=list(levels(houseprice$customf)), 
    points=tpg,
    border=1, 
    title="custom", 
    cex=1),
  group=houseprice$customf, 
  data=houseprice,
  main="houseprice by custom | corner", 
  par.strip.text=list(cex=1.5),
  superpanel=panel.pairs, 
  subpanel.scales=list(cex=.8), pscales=4, 
  axis.text.cex=.7,
  panel.cex=1.2, 
  pch=c(15,16))

# output
houseprice.lm <- lm(price ~ sqft + custom + 
                      corner + taxes, 
                      data=houseprice)
summary(houseprice.lm)

houseprice.lm1 <- lm(price ~ sqft + taxes, 
                     data=houseprice) 
summary(houseprice.lm1)

#Partial F-test
anova(houseprice.lm1, houseprice.lm)

#multicollinearity 
vif(houseprice.lm)

# Prediction and Confidence Intervals
pi.fit <- predict(houseprice.lm, 
                  newdata=data.frame(sqft=2000, 
                        taxes = 1850, 
                        custom = 0, corner = 0),
                  se.fit=TRUE, 
                  interval = "prediction")
pi.fit

ci.fit <- predict(houseprice.lm, 
                  newdata=data.frame(sqft=2000, 
                        taxes = 1850, 
                        custom = 0, corner = 0),
                  se.fit=TRUE, 
                  interval = "confidence")
ci.fit
