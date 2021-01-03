library(HH)
data(hpErie)
head(hpErie)
summary(hpErie)

splom( ~ hpErie, pch=16, pscales=4, varname.cex=.6, axis.text.cex=.3)

hpErie.lm <- lm(price ~., data = hpErie)## full model
summary(hpErie.lm)
vif(hpErie.lm)

residual.plots.lattice(hpErie.lm, par.strip.text=list(cex=1.1))
hpErie

## before running our stepwise regression we need to reformate 
## our dataset see comments at the end of this file for 
## an explaination.

hpErie.mm <- data.frame(price=hpErie$price, 
                        model.matrix(hpErie.lm)[,-1])

names(hpErie.mm) ## reformates categorical data
head(hpErie.mm) 

hpErie.mm.subsets <-
  leaps::regsubsets(price ~ ., data=hpErie.mm, nbest=2)
hpErie.mm.subsets.summary <- summaryHH(hpErie.mm.subsets)
hpErie.mm.subsets.summary

hpErie.mm.lm.11 <- lm.regsubsets(hpErie.mm.subsets, 11)
summary(hpErie.mm.lm.11)
vif(hpErie.mm.lm.11)

hpErie.mm.lm.9 <- lm.regsubsets(hpErie.mm.subsets, 9)
summary(hpErie.mm.lm.9)
vif(hpErie.mm.lm.9)

hpErie.mm.lm.7 <- lm.regsubsets(hpErie.mm.subsets, 7)
summary(hpErie.mm.lm.7)
vif(hpErie.mm.lm.7)

## manual selection full model
hpErie.lm <- lm(price ~ taxes + bathrm + lotsize + sqfeet + 
                  garage + rooms + bedrm + age + typebrick.frame + 
                  typealum.frame + typeframe + style1.5.story + 
                  styleranch + fireplac, 
                  data = hpErie.mm)
summary(hpErie.lm)
vif(hpErie.lm)

## remove bedrm
hpErie.lm.1 <- lm(price ~ taxes + bathrm + lotsize + sqfeet + 
                garage + rooms + age + typebrick.frame + 
                typealum.frame + typeframe + style1.5.story + 
                styleranch + fireplac, 
                data = hpErie.mm)
summary(hpErie.lm.1)
vif(hpErie.lm.1)

## remove bathrm
hpErie.lm.2 <- lm(price ~ taxes + lotsize + sqfeet + 
                garage + rooms + age + typebrick.frame + 
                typealum.frame + typeframe + style1.5.story + 
                styleranch + fireplac, 
                  data = hpErie.mm)
summary(hpErie.lm.2)
vif(hpErie.lm.2)

## remove room
hpErie.lm.3 <- lm(price ~ taxes + lotsize + sqfeet + 
                  garage + age + typebrick.frame + typealum.frame +
                  typeframe + style1.5.story + 
                  styleranch + fireplac, 
                  data = hpErie.mm)
summary(hpErie.lm.3)
vif(hpErie.lm.3)

## remove taxes
hpErie.lm.4 <- lm(price ~ lotsize + sqfeet + 
                  garage + age + typebrick.frame + typealum.frame +
                  typeframe + style1.5.story + 
                  styleranch + fireplac, 
                  data = hpErie.mm)
summary(hpErie.lm.4)
vif(hpErie.lm.4)

## remove styleranch
hpErie.lm.5 <- lm(price ~ lotsize + sqfeet + 
                  garage + age + typebrick.frame + typealum.frame +
                  typeframe + style1.5.story + fireplac, 
                  data = hpErie.mm)
summary(hpErie.lm.5)
vif(hpErie.lm.5)

## remove lotsize
hpErie.lm.6 <- lm(price ~ sqfeet + 
                  garage + age + typebrick.frame + typealum.frame +
                  typeframe + style1.5.story + fireplac, 
                  data = hpErie.mm)
summary(hpErie.lm.6)
vif(hpErie.lm.6)

## remove style1.5.story
hpErie.lm.7 <- lm(price ~ sqfeet + 
                  garage + age + typebrick.frame + typealum.frame +
                  typeframe + fireplac, 
                  data = hpErie.mm)
summary(hpErie.lm.7)
vif(hpErie.lm.7)

## remove garage
hpErie.lm.8 <- lm(price ~ sqfeet + 
                  age + typebrick.frame + typealum.frame +
                  typeframe + fireplac, 
                  data = hpErie.mm)
summary(hpErie.lm.8)
vif(hpErie.lm.8)

##Residual Plots for best model from manual process 
lmplot(hpErie.lm.8)
names(hpErie.lm.8)

hpErie.lm.8$residuals
hpErie.lm.8$residuals[c(6,11,13)]
hpErie.lm.8$fitted.values[c(6,11,13)]
hpErie$price[c(6,11,13)]

##Diagnostics for best model 
hpErie.lm.8.case <- case(hpErie.lm.8) 
hpErie.lm.trellis <-
  plot(hpErie.lm.8.case, hpErie.lm.8, par.strip.text=list(cex=1.2), 
       layout=c(4,3), main.cex=1.6)
hpErie.lm.trellis

## stepwise regression code - explains why data was reformated
## hpErie.subsets <-
##  leaps::regsubsets(price ~ ., data=hpErie, nbest=2)
## hpErie.subsets.summary <- summaryHH(hpErie.subsets)
## hpErie.subsets.summary
## tmp <- (hpErie.subsets.summary$cp <= 10)
## hpErie.subsets.summary[tmp,]

##Please Note the following error and how to correct it

## hptErie.lm.7 <- lm.regsubsets(hpErie.subsets, 7) ## this has error
## Error in eval(expr, envir, enclos) : object 'typebrick' not found
##
## There are two problems
##
## 1. The individual dummy variables are selected by regsubsets, not
## the full set of dummy variables for a factor.
##
## 2. The generated dummy variable name "typebrick&frame" includes the
## character "&", and is therefore a non-syntactic name.
##
## The repair is two steps.
##
## A. Use the model matrix (with the dummy variables), not the
## original data.frame.  The model matrix is a matrix.  When we
## construct the new data.frame based on the model.matrix, the names
## are converted to syntactic names, in this case "typebrick.frame".
##