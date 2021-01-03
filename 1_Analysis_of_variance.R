# Script file for One-Way ANOVA

library(HH)

data(notch)
str(notch)
head(notch)
summary(notch)

# simple boxplot
bwplot(energy ~ machine, data=notch)

# boxplot with pars      
bwplot(energy ~ machine, data=notch, 
  panel=panel.bwplot.superpose, 
  groups=machine, 
  ylab=list("energy"),
  xlab=list("machine"))

# One-Way ANOVA model
notch.fix.aov <- aov(energy ~ machine, 
                     data=notch) 

anova(notch.fix.aov) #displays model output

# table of means
model.tables(notch.fix.aov, "means")

# multiple comparison tests
notch.fix.mmc <- mmc(notch.fix.aov, 
                     linfct = mcp(
                       machine = "Tukey")) 
notch.fix.mmc

# Creates, displays and graphs 
# Mean minus Mean comparisons
mmcplot(notch.fix.mmc, style = "both")

# Homogeneity of Variance test and plot
hovBF(energy ~ machine, data=notch)

hovplotBF(energy ~ machine, data=notch)
