#Script file for Data Displays

library(HH) 
data(njgolf) 
head(njgolf)

# create tmp a subset of variables 
# and factor labels
tmp <- cbind(njgolf[,c("sprice","lotsize","beds",
                       "drarea","kitarea")], 
  cond.house=factor(njgolf$lotsizef=="house",
             labels=c("condominium","house"))) 
head(tmp)
summary(tmp)

# scatterplot of  with no adjustments 
splom(~ tmp)

# scatterplot of njgolf
splom(~ tmp, axis.text.cex=.5, varname.cex=.8, 
      xlab=NULL, pch=16, group=tmp$cond.house, 
      key=list(border=TRUE, 
             text=list(c("Condominium","House")), 
             points=list(col=trellis.par.get(
               "superpose.symbol")$col[1:2],
               pch=16), 
             space="bottom", columns=2))

# scatterplot with condominium and 
# house separated no adjustments 
splom(~ tmp[,1:5] | tmp$cond.house)

# same as 
#splom(~ tmp[,1:5] | tmp[,6])

# scatterplot of njgolf with condominium and 
# house separated 
splom(~ tmp[,1:5] | tmp$cond.house, 
      par.strip.text=list(cex=1.5),
      axis.text.cex=.5, varname.cex=.8, 
      xlab=NULL, pch=19, groups=tmp$cond.house)

# life expectancy
data(tv) 
head(tv)
abbreviate(row.names(tv))

# xyplot
xyplot(male.life.exp ~ fem.life.exp, data=tv)

# xyplot with added features and adjustments 
xyplot(male.life.exp ~ fem.life.exp, data=tv,
    main="Life Expectancy", xlab="Female", 
    ylab="Male", pch=19, aspect="iso",
    xlim=c(48,90), ylim=c(48,90)) + 
  layer(panel.abline(a=0, b=1))

# life expectancy with labeled points 
xyplot(male.life.exp ~ fem.life.exp, data=tv,
    xlim=c(48,85), ylim=c(48,85), aspect="iso", 
    panel=panel.text, 
    labels=abbreviate(row.names(tv)), cex=.8, 
    col=trellis.par.get(
      "superpose.symbol")$col[1], 
    main="a. abbreviated names") +
  layer(panel.abline(a=0, b=1))
