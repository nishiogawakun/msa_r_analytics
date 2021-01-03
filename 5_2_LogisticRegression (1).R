# Lymph Nodes Example 
library(HH)

data(lymph) 
head(lymph)

# First Graph, Raw Data 
col2 <- likertColor(2)[2:1] 
useOuterStripsT2L1(
  xyplot(age ~ acid.ph | grade * stage * X.ray, 
         data=lymph, 
         group=nodes, pch=levels(lymph$nodes), 
         col=col2, cex=2.2, 
         layout=c(4, 2),
      # useOuterStrips2L1 adjust for 3 factors 
      # (categorial) variables if there are only       # 2 the function useOuterStrips should 
      # be used instead
      
      # above code is necessary
      # the code below makes it prettier
        main=list("age ~ acid.ph | grade 
                   * stage * X.ray, group=nodes", 
                   cex=1.6),
         aspect=1,
         between=list(x=c(.5, 1, .5), y=1), 
         scales=list(cex=1, alternating=FALSE), 
         xlab=list(cex=1.4), ylab=list(cex=1.4), 
         par.strip.text=list(cex=1.25), 
         key=list(space="right",
                  text=list(levels(lymph$nodes), 
                            cex=1.5, adj=1, 
                            col=col2), 
                  columns=2, border=1,
                  title="nodes", cex.title=1.25, 
                  cex=1)))

# models to consider
# full model
lymph1.glm <- glm(nodes ~ X.ray + stage + grade 
                  + acid.ph + age, 
                  data=lymph, family=binomial)
summary(lymph1.glm)

# age is deleted
lymph2.glm <- glm(nodes ~ X.ray + stage + grade 
                  + acid.ph,
                  data=lymph, family=binomial) 
summary(lymph2.glm)

## age and grade are deleted
lymph3.glm <- glm(nodes ~ X.ray + stage + acid.ph,
                  data=lymph, family=binomial) 
summary(lymph3.glm)

## age and grade and acid.ph are deleted
lymph4.glm <- glm(nodes ~ X.ray + stage,
                  data=lymph, family=binomial) 
summary(lymph4.glm)

# predicted probabilities, logit and odds
p.hat <- predict.glm(lymph3.glm, type="response") 
logit.p.hat <- logit(p.hat)
odds.hat <- p.hat/(1-p.hat)

# needed for next graphic
lhat <- cbind(lymph, p.hat=p.hat, 
              odds.hat=odds.hat, 
              logit.p.hat=logit.p.hat) 
lhat.sort <- lhat[with(lhat, 
                       order(X.ray, stage, 
                             acid.ph)),] 
lhat.sort$Xsg <- with(lhat.sort, 
              interaction(X.ray, stage)) 
lhat.sort$Xsg <- factor(lhat.sort$Xsg, 
              levels=unique(lhat.sort$Xsg))

p8d <-
  xyplot(nodes.j ~ acid.ph | stage + X.ray, 
         data=lhat.sort, 
         layout=c(2,2), 
         between=list(x=c(.5, 1, .5), y=1.5), 
         groups=lhat.sort$nodes,
         pch=levels(lhat.sort$nodes), 
         col=col2, cex=2.2,
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
         text=list(levels(lymph$nodes), 
                  cex=1.5, adj=1, 
                  col=col2),
                  columns=2,
                  border=1,
                  title="nodes", cex.title=1.25,
                  cex = 1))

ul35 <- unique(lymph[, 3:4])
ul35 <- ul35[with(ul35, 
                  order(X.ray, stage)),] 
ul35$Xsg <- with(ul35, 
                interaction(X.ray, stage)) 
ul35$Xsg <- factor(ul35$Xsg, 
                   levels=unique(ul35$Xsg))
tmp <- lapply(1:4, function(i) 
  cbind(acid.ph=29:198, ul35[i,])) 
tmp2 <- do.call("rbind", tmp)
tmp2$nodes.hat <- predict.glm(lymph3.glm, 
                              type="response", 
                              newdata=tmp2)

p8e <- xyplot(nodes.hat ~ acid.ph |	 
                stage + X.ray, data=tmp2, 
              layout=c(2,2),type="l")

useOuterStrips(p8d + p8e +
          layer(panel.abline(h=c(0,1),
                                lty=2, 
                                col="gray60")),
          strip=strip.custom(strip.names=
                                    c(TRUE,TRUE)),                     , 
          strip.left= strip.custom(strip.names=
                                     TRUE))
# Again, useOuterStrips2L1 adjust for 3 factors 
# (categorial variables) if there are only 2 
# the function useOuterStrips should be used 
# instead


# Predict new patient age=51 with X-ray=0, 
# grade=1, stage=1, acid.ph=71 
predict.glm(lymph3.glm, type="response",
            newdata=data.frame(acid.ph=71, 
                               X.ray= "0", 
                               stage= "1", 
                               grade= "1", 
                               age = 51))
summary(lymph3.glm)

