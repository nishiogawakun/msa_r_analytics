library(HH)
require(vcd)

data(crime)

crime

colSums(crime)
rowSums(crime)

prop.arson <- crime["arson",]/colSums(crime)
prop.rape <- crime["rape",]/colSums(crime)
prop.violence <- crime["violence",]/colSums(crime)
prop.stealing <- crime["stealing",]/colSums(crime)
prop.coining <- crime["coining",]/colSums(crime)
prop.fraud <- crime["fraud",]/colSums(crime)

prop.drinker <- crime[,"drinker"]/rowSums(crime)
prop.abstainer <- crime[,"abstainer"]/rowSums(crime)

mosaic(crime)

mosaic(t(crime), direction=c("v","h"),
       gp=gpar(fill=likertColor(6), 
               col="transparent"),
       rot_labels=c(0,0,0,0),  
       # zero is horizontal
       rot_varnames=c(0,0,0,0),
       offset_labels=c(0, -0.6, 0, 1), 
       offset_varnames=c(0, -0.6, 0, 2.4),
       margins=c(left=6.5),
       keep_aspect_ratio=FALSE)

mosaic(crime, direction=c("v","h"),
       gp=gpar(fill=likertColor(2), col="transparent"),
       rot_labels=c(0,0,0,0),  ## zero is horizontal
       rot_varnames=c(0,0,0,0),
       offset_labels=c(0, -0.6, 0, 1), 
       offset_varnames=c(0, -0.6, 0, 2.4),
       margins=c(left=6.5),
       keep_aspect_ratio=FALSE)

crime.chisq <- chisq.test(crime)
crime.chisq

crime.chisq$observed
crime.chisq$expected
crime.chisq$residuals

crime.chisq$residuals^2

barchart(Freq ~ crime | perpetrator, 
         as.data.frame(crime.chisq$residuals),
         origin=0, layout=c(1,2), as.table=TRUE, 
         scales=list(alternating=2), 
         ylab=list("Freq", rot = 0),
         ylab.right=list("Residual", rot=0), 
         xlab=list("Age", rot=0), 
         xlab.right=list("Age", rot=0),
         strip=FALSE, strip.left=TRUE)
