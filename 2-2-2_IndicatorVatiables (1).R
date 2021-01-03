## Script file for Indicator Variables

## hotdog data set

## Reference: Moore, David S., and George P. McCabe (1989). Introduction
## to the Practice of Statistics. Original source: Consumer Reports, June
## 1986, pp. 366-367.

## Description: Results of a laboratory analysis of calories and
## sodium content of major hot dog brands. Researchers for Consumer
## Reports analyzed three types of hot dog: beef, poultry, and meat
## (mostly pork and beef, but up to 15% poultry meat).

## Number of cases: 54

## Variable Names:
##	Type: Type of hotdog (beef, meat, or poultry)
## Calories: Calories per hot dog
##	Sodium: Milligrams of sodium per hot dog

library(HH) 
data(hotdog) 
head(hotdog)

## boxplot and oneway analysis of variance 
bwplot(Sodium ~ Type, data=hotdog) 
hotdog.aov <- ancova(Sodium ~ Type,
  data=hotdog, x=Calories, 
  par.strip.text=list(cex=1.2), 
  ylim=c(140,700))
summary(hotdog.aov) 
model.tables(hotdog.aov, type="means")

## horizontal lines: zero slope and separate intercepts 
print(position=c(0,0, 1,.6),
  attr(hotdog.aov,"trellis"))

## multiple comparisons of ANOVA
hotdog.mca <- glht(hotdog.aov, linfct =   mcp(Type="Tukey")) 
  print(hotdog.mca)
  old.omd <- par(omd=c(.1,1, 0,1)) 
plot(hotdog.mca)
par(old.omd)

## regression, same line: common intercept and common slope 
hC.aov <- ancova(Sodium ~ Calories, groups=Type, data=hotdog,
                          par.strip.text=list(cex=1.2), ylim=c(140,700)) 
print(position=c(0,0, 1,.6),
             attr(hC.aov,"trellis")) 
summary.aov(hC.aov)
coef(hC.aov)

## analysis of covariance
## analysis with a concomitant explanatory variable
## parallel lines: separate intercepts and common slope 
hCT.aov <- ancova(Sodium ~ Calories + Type, data=hotdog,
                      par.strip.text=list(cex=1.2), ylim=c(140,700)) 
print(position=c(0,0, 1,.6),
       attr(hCT.aov,"trellis")) 
summary(hCT.aov)

## multiple comparisons of ANCOVA
hotdog.mca <- glht(hCT.aov, linfct=mcp(Type="Tukey")) 
print(hotdog.mca)

old.omd <- par(omd=c(.1,1, 0,1)) 
plot(hotdog.mca)
par(old.omd)

##Regression Coefficents of parallel lines: separate intercepts and common slope 
coef(summary.lm(hCT.aov))

## interaction: separate intercepts and slopes
hCTi.aov <- ancova(Sodium ~ Calories * Type, data=hotdog,
                       par.strip.text=list(cex=1.2), ylim=c(140,700)) 
print(position=c(0,0, 1,.6),
       attr(hCTi.aov,"trellis")) 
summary(hCTi.aov) 
coef(summary.lm(hCTi.aov))

##Conclusion/Summary of code

## y ~ x	## constant line across all groups 
ancova(Sodium ~ Calories,	data=hotdog, groups=Type)

## y ~ a	## different horizontal line in each group 
ancova(Sodium ~ Type, data=hotdog, x=Calories)

## y ~ x + a	or	y ~ a + x	## constant slope, different intercepts 
ancova(Sodium ~ Calories + Type, data=hotdog)
ancova(Sodium ~ Type + Calories, data=hotdog)

## y ~ x * a	or	y ~ a * x	## different slopes, and different intercepts 
ancova(Sodium ~ Calories * Type, data=hotdog)
ancova(Sodium ~ Type * Calories, data=hotdog)
