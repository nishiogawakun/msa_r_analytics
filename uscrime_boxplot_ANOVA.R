# The uscrime data file is explained in 
# detail in exercise 4.3
install.packages("HH")
library(HH)
data(uscrime)
head(uscrime)
tail(uscrime)
summary(uscrime)

uscrime$S <- as.factor(uscrime$S)
summary(uscrime$S)

# boxplot of crime rate with 
# Southern/ Non-Southern states
boxplot(R ~ S, data=uscrime)
bwplot(R ~ S, data=uscrime) # from HH

# boxplot from HH with author's suggestions
bwplot(R ~ S, data=uscrime,
       panel=panel.bwplot.superpose, 
       groups=S,
       ylab=list("Crime Rate"),
       xlab=list("Southern States"))

# incorrect results
data(uscrime) # recall data 
summary(uscrime$S) # S is not a factor
bwplot(R ~ S, data=uscrime) # from HH

# the correct way
uscrime$S <- as.factor(uscrime$S)
summary(uscrime$S)

bwplot(R ~ S, data=uscrime,
       panel=panel.bwplot.superpose, groups=S,
       ylab=list("Crime Rate"),
       xlab=list("Southern States"))
uscrime.aov <- aov(R ~ S, data=uscrime)
anova(uscrime.aov)

model.tables(uscrime.aov, "means")

hovBF(R ~ S, data=uscrime)
# Homogeneity of Variance test 

hovplotBF(R ~ S, data=uscrime)
# Homogeneity of Variance plot 


# ANOVA example of differences

bwplot(Ed ~ S, data=uscrime,
       panel=panel.bwplot.superpose, 
       groups=S,
       ylab=list("Ed"),
       xlab=list("Southern States"))

uscrime_Ed.aov <- aov(Ed ~ S, data=uscrime)
anova(uscrime_Ed.aov)

model.tables(uscrime_Ed.aov, "means")
uscrime_Ed.mmc <- mmc(uscrime_Ed.aov, 
                      linfct = mcp(S = "Tukey")) 
uscrime_Ed.mmc
mmcplot(uscrime_Ed.mmc, style = "both")

hovBF(Ed ~ S, data=uscrime)
hovplotBF(Ed ~ S, data=uscrime)
