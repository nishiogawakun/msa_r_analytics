## Script file for Chi- Square 

library(HH)
require(vcd)## required package for new plots

data(drunk) 
drunk

##proportion of males and females 
prop.male <- drunk["males",]/colSums(drunk)
prop.male
prop.female <- drunk["females",]/colSums(drunk) 
prop.female

##proportion of males and females in bad barchart
ages <- ordered(dimnames(drunk)$age, levels=dimnames(drunk)$age) 
barchart(prop.male ~ ages,
  horizontal=FALSE, origin=0, 
  ylab="", main="proportion male")

barchart(prop.female ~ ages,
  horizontal=FALSE, origin=0, 
  ylab="", main="proportion female")

## improved graphic
mosaic(t(drunk), direction=c("v","h"), 
  gp=gpar(fill=likertColor(2), 
  col="transparent"), rot_labels=c(0,0,0,0),	
  ## zero is horizontal 
  rot_varnames=c(0,0,0,0),
  offset_labels=c(0, -0.6, 0, 1),
  offset_varnames=c(0, -0.6, 0, 2.4), 
  margins=c(left=6.5), 
  keep_aspect_ratio=FALSE)

drunk.chisq <- chisq.test(drunk) 
drunk.chisq
drunk.chisq$observed 
drunk.chisq$expected
drunk.chisq$residuals	## cell chi values 
drunk.chisq$residuals^2 ## cell chi-square values

##calculation of residual for female >=60
## (observed minus expected) / sqrt(expected) 
(10-3.763464)/sqrt(3.7634464)

## barchart of residuals
barchart(Freq ~ age | sex, as.data.frame(drunk.chisq$residuals),
  origin=0, layout=c(1,2), as.table=TRUE, 
  scales=list(alternating=2), ##between=list(y=1), 
  ylab=list("sex", rot=0),
  ylab.right=list("Chi values", rot=0), xlab="Age", 
  strip=FALSE, strip.left=TRUE)
