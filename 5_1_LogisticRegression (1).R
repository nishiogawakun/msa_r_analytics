# Script file for Logistic Regression 

library(HH)

# Space Shuttle Example 
data(spacshu) 
head(spacshu)

# Visual of Raw Data incorrect 
# cannot see overlap
xyplot(damage ~ tempF,
       ylab="damage", 
       data=spacshu,
       scales=list(y=list(at=c(0,1))), 
       cex=.5, pch=16,
       main="Observed")

# Corect visual of Raw Data

Jitter.factor <- .08
# jitter factor needed to see multiple 0/1 in 
# binary coding for damage 

xyplot(jitter(damage, factor=Jitter.factor*8) 
       ~ tempF,
  ylab="damage", 
  data=spacshu,
  scales=list(y=list(at=c(0,1))), 
  cex=.5, pch=16,
  main="a. observed", 
  panel=function(...) {
    panel.xyplot(...) 
    panel.abline(h=0:1, lty=2)
    })

# Logistic Model
spacshu.bin.glm <- glm(damage ~ tempF, 
                       data=spacshu, 
                       family=binomial) 

summary(spacshu.bin.glm)

# prediction on response scale, in this 
# case (0,1) leading to Figure spaceshuttle
# Panel d in Fig. 17.3 

spacshu.pred <-
  interval(spacshu.bin.glm, 
           newdata=data.frame(tempF=30:85), 
  type="response")

head(cbind(tempF=30:85, spacshu.pred))

cbind(tempF=30:85, round(spacshu.pred, 
                         digits=2))[c(1:3,54:56),] 

p <- spacshu.pred[,"fit"]

# logit fit estimating p(damage in one ring)
xyplot(jitter(damage, factor=Jitter.factor*4) 
       ~ tempF, data=spacshu, 
  ylab="proportion damaged",
  main="GLM logit fit with pi, estimating 
  p(damage in one ring)", 
  xlim=c(30, 85), ylim=c(-.1, 1.5), 
  scales=list(y=list(at=c(0,1))),
  cex=.5, pch=16, 
  panel=function(...) { 
    panel.xyplot(...) 
    panel.lines(x=30:85, y=p,
    lty=1, 
    col=trellis.par.get("superpose.line")$col[1])
    panel.lines(x=30:85, y=spacshu.pred[,"pi.low"], 
    lty=4,
    col=trellis.par.get("superpose.line")$col[4]) 
    panel.lines(x=30:85, y=spacshu.pred[,"pi.hi"],
    lty=4, 
    col=trellis.par.get("superpose.line")$col[4])
    panel.abline(h=0:1, lty=2)
    }
  )


#formatting needed to create 3 panel graph 
spacshu6 <- data.matrix(spacshu) 
dimnames(spacshu6) <- NULL 
dim(spacshu6) <- c(6,23,2)
spacshu6 <- data.frame(damage=apply(spacshu6[,,2],
                                    2,sum), 
                       tempF=spacshu6[1,,1])

tmp <- cbind.data.frame(tempF=30:85,
  p=p, 
  odds=p/(1-p),
  logit.p=logit(p),
  which="fit", stringsAsFactors=FALSE)
  p.obs <- spacshu6[,1]/6

  tmp2 <- cbind.data.frame(spacshu6[,2,
                                    drop=FALSE],
  p=p.obs, 
  odds=p.obs/(1-p.obs), 
  logit.p=logit(p.obs),
  which="data", stringsAsFactors=FALSE) 
  tmp2$logit.p[tmp2$p==0] <- 
    min(tmp2$logit.p[tmp2$p!=0])-2.5	
  ## pull infinity in
  tmp.both <- rbind(tmp2,tmp)

# graph of p, odds and logit(p)for space shuttle data 
xyplot(p + odds + logit.p ~ tempF, 
       data=tmp.both,
  type=c("p","l"), groups=rep(tmp.both$which,3),
  pch=19, 
  distribute.type=TRUE,
  layout=c(1, 3),
  ylim=list(c(0, 1), c(0, 5), c(-4, 2)), 
  par.strip.text=list(cex=1.4),
  scales=list(cex=1, 
              y=list(relation="free", 
                     tick.number=4), 
  alternating=FALSE),
  between=list(y=1.5), 
  xlab="Temperature Fahrenheit", ylab="")
