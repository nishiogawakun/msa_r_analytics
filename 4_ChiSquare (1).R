# Script file for Chi- Square 

library(HH)
require(vcd)## required package for new plots

data(selfexam)
selfexam

selfexam[1,] # lists the first row
selfexam[,1] # lists the first column

colSums(selfexam) # sums frequencies
rowSums(selfexam) # sums ages

# proportion by frequency 
prop.lessthan45 <- selfexam["<45",]/
  colSums(selfexam)
prop.lessthan45 

prop.45to59 <- selfexam["45-59",]/
  colSums(selfexam)
prop.45to59

prop.60plus <- selfexam["60+",]/
  colSums(selfexam)
prop.60plus

# proportions by age
prop.m <- selfexam[,"monthly"]/ rowSums(selfexam)
prop.m

prop.o <- selfexam[,"occasionally"]/ 
  rowSums(selfexam)
prop.o

prop.n <- selfexam[,"never"]/ rowSums(selfexam)
prop.n

# basic mosaic plot
mosaic(selfexam)

# mosic plot with parameters
mosaic(t(selfexam), direction=c("v","h"),
       gp=gpar(fill=likertColor(3), 
               col="transparent"),
       rot_labels=c(0,0,0,0),  
       # zero is horizontal
       rot_varnames=c(0,0,0,0),
       offset_labels=c(0, -0.6, 0, 1), 
       offset_varnames=c(0, -0.6, 0, 2.4),
       margins=c(left=6.5),
       keep_aspect_ratio=FALSE)

# improved visual
mosaic(selfexam, direction=c("v","h"),
       gp=gpar(fill=likertColor(3), col="transparent"),
       rot_labels=c(0,0,0,0),  ## zero is horizontal
       rot_varnames=c(0,0,0,0),
       offset_labels=c(0, -0.6, 0, 1), 
       offset_varnames=c(0, -0.6, 0, 2.4),
       margins=c(left=6.5),
       keep_aspect_ratio=FALSE)

selfexam.chisq <- chisq.test(selfexam)
selfexam.chisq

selfexam.chisq$observed
selfexam.chisq$expected

selfexam.chisq$residuals

# calculation of residual for monthly, age <45
# (observed minus expected) / sqrt(expected) 
(91-66.77632)/sqrt(66.77632)

# residual squared
selfexam.chisq$residuals^2

((91-66.77632)/sqrt(66.77632))^2

## barchart of residuals
barchart(Freq ~ age | frequency, 
         as.data.frame(selfexam.chisq$residuals),
         origin=0, layout=c(1,3), as.table=TRUE, 
         scales=list(alternating=2), 
         ylab=list("Freq", rot = 0),
         ylab.right=list("Residual", rot=0), 
         xlab=list("Age", rot=0), 
         xlab.right=list("Age", rot=0),
         strip=FALSE, strip.left=TRUE)

