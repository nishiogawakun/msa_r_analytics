# Script file for Two-Way ANOVA

library(HH)

# Fixed vs Random Factors 
data(display)
head(display)
summary(display)

# Interaction plot
interaction2wt(time ~ panel.ordered + emergenc, 
               data=display, 
  par.strip.text=list(cex=.8))

# Two-Way ANOVA Fixed Factors
displayf.aov <- aov(time ~ emergenc * panel, 
                    data=display) 
anova(displayf.aov)

# interaction is not significant
displayf.aov.noint <- aov(time ~ emergenc + panel, 
                          data=display) 
anova(displayf.aov.noint)

# multiple comparison tests and plot
displayf.mmc <- mmc(displayf.aov.noint, 
                    focus="panel") 
displayf.mmc
mmcplot(displayf.mmc, style="both")

# table of means
model.tables(displayf.aov, "means")

# Two-Way ANOVA Random Factors
displayr.aov <- aov(time ~ Error(emergenc/panel) 
                    + panel,
                        data=display) 
summary(displayr.aov)
