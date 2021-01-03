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
