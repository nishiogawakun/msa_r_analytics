##Randomized Complete Block Design code

library(HH)
data(mpg)
head(mpg)
interaction2wt(mpg ~ day + car, data=mpg)
## correct spacing of car on graph
position(mpg$car) <- c(1,3,5,7,9)
interaction2wt(mpg ~ day + car, data=mpg)

## additional view of the effect of the car factor
## conditioned on day
xyplot(mpg ~ car | day, data=mpg, type="b", pch=19, layout=c(1, 10),
       strip=FALSE, strip.left=TRUE)

##identify factors - ensures correct dfs
mpg$car <-
  factor(mpg$car, order=FALSE)
mpg$day <-
  factor(mpg$day)

mpg.aov <- aov(mpg ~ Error(day) + car, data=mpg)
summary(mpg.aov)

mpg.aov2 <- aov(mpg ~ car + day, data=mpg)
mmcplot(mmc(mpg.aov2, focus="car"), style="both")
