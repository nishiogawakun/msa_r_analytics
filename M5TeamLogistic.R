## logistic Regression Script File
library(HH)
data(icu)
head(icu)
summary(icu)

icu$SER <- as.factor(icu$SER)
summary(icu)

# taking only the columns that we are interested in
icu.small <- icu[,c('STA', 'AGE', 'SEX', 'SER', 'CPR', 'HRA', 'jSTA')]
head(icu.small)
summary(icu.small)
str(icu.small)

splom(~ icu.small, pch=16, alpha=0.4, 
      pscales=2, 
      varname.cex=.8, 
      axis.text.cex=.5)

# First Graph, Raw Data 
col2 <- likertColor(2)[2:1] 
useOuterStripsT2L1(
  xyplot(AGE ~ HRA | SEX * SER * CPR, 
         data=icu.small, 
         group=STA, pch=levels(icu.small$STA), 
         col=col2, cex=1.9, 
         layout=c(4, 2),
         # useOuterStrips2L1 adjust for 3 factors 
         # (categorial) variables if there are only       # 2 the function useOuterStrips should 
         # be used instead
         
         # above code is necessary
         # the code below makes it prettier
         main=list("AGE ~ HRA | SEX * SER * CPR,
                   group=STA", 
                   cex=1),
         aspect=1,
         between=list(x=c(.5, 1, .5), y=1), 
         scales=list(cex=1, alternating=FALSE), 
         xlab=list(cex=1.2), ylab=list(cex=1.2), 
         par.strip.text=list(cex=1), 
         key=list(space="right",
                  text=list(levels(icu.small$STA), 
                            cex=1, adj=1, 
                            col=col2), 
                  columns=2, border=1,
                  title="STA", cex.title=1, 
                  cex=1)))

# models to consider
# full model
icu.small.glm <- glm(STA ~ AGE + SEX + SER + CPR + HRA, 
                  data=icu.small, family=binomial)
summary(icu.small.glm)

# SEX deleted
icu.small2.glm <- glm(STA ~ AGE +  HRA + SER + CPR, 
                     data=icu.small, family=binomial)
summary(icu.small2.glm) 

# HRA deleted
icu.small3.glm <- glm(STA ~ AGE + SER + CPR, 
                      data=icu.small, family=binomial)
summary(icu.small3.glm) 

# ANOVA
anova(icu.small3.glm, test="Chisq")

# predicted probabilities, logit and odds
p.hat <- predict.glm(icu.small3.glm, type="response") 
logit.p.hat <- logit(p.hat)
odds.hat <- p.hat/(1-p.hat)

# needed for next graphic
lhat <- cbind(icu, p.hat=p.hat, 
              odds.hat=odds.hat, 
              logit.p.hat=logit.p.hat) 
lhat.sort <- lhat[with(lhat, 
                       order(AGE, SER, CPR)),] 
lhat.sort$Xsg <- with(lhat.sort, 
                      interaction(SER, CPR)) 
lhat.sort$Xsg <- factor(lhat.sort$Xsg, 
                        levels=unique(lhat.sort$Xsg))
# visual
p8d <-
  xyplot(jSTA ~ AGE | SER + CPR, 
         data=lhat.sort, 
         layout=c(2,2), 
         between=list(x=c(.5, 1, .5), y=1.5), 
         groups=lhat.sort$STA,
         pch=levels(lhat.sort$STA), 
         col=col2, cex=1.9,
         main=list("jittered observed and 
                   predicted probability(nodes)", 
                   cex=1.6),
         # above is necessary, below makes 
         # the graphic prettier
         scales=list(cex=1, alternating=FALSE, 
                     y=list(at=c(0, .25, .5, .75, 1))),
         xlab=list(cex=1.4), ylab=list(cex=1.4),
         par.strip.text=list(cex=1.4),
         strip=strip.custom(strip.names=
                              c(TRUE,TRUE)), 
         key=list(space="right",
                  text=list(levels(icu$STA), 
                            cex=1.5, adj=1, 
                            col=col2),
                  columns=2,
                  border=1,
                  title="STA", cex.title=1.25,
                  cex = 1))

ul35 <- unique(icu.small[, 4:5])
ul35 <- ul35[with(ul35, 
                  order(SER, CPR)),] 
ul35$Xsg <- with(ul35, 
                 interaction(SER, CPR)) 
ul35$Xsg <- factor(ul35$Xsg, 
                   levels=unique(ul35$Xsg))
tmp <- lapply(1:4, function(i) 
  cbind(AGE=15:100, ul35[i,])) 
tmp2 <- do.call("rbind", tmp)
tmp2$STA.hat <- predict.glm(icu.small3.glm, 
                              type="response", 
                              newdata=tmp2)

p8e <- xyplot(STA.hat ~ AGE | SER + CPR, data=tmp2, 
              layout=c(2,2),type="l")

useOuterStrips(p8d + p8e +
                 layer(panel.abline(h=c(0,1),
                                    lty=2, 
                                    col="gray60")),
               strip=strip.custom(strip.names=
                                    c(TRUE,TRUE)),                     
               strip.left= strip.custom(strip.names=
                                          TRUE))
# Again, useOuterStrips2L1 adjust for 3 factors 
# (categorial variables) if there are only 2 
# the function useOuterStrips should be used 
# instead

# diagnostic plots...seem useless
lmplot(icu.small3.glm)

icu.small3.glm.case <- case(icu.small3.glm) 
icu.small3.glm.trellis <-
  plot(icu.small3.glm.case, icu.small3.glm, par.strip.text=list(cex=1.2), 
       layout=c(4,4), main.cex=1.6)
icu.small3.glm.trellis


# Predictions
predict.glm(icu.small3.glm, type="response",
            newdata=data.frame(AGE = 91,
                               SER = "0",
                               CPR = "Yes"))
summary(lymph3.glm)

