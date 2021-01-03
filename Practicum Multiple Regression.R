library(HH) 
library(dplyr)
library(fastDummies)
library(glmnet)

ds <- read.csv("Combined Sales.csv")
ds <- select (ds,-c(Short_Product_Description, Brand_Low, Brand_High, UPC, All_Products, 
                    BC_Category, Growing_Area))
ds <- filter(ds, All_Periods == 'Latest 52 Wks - W/E 10/06/18')
ds <- filter(ds, BC_Segment %in% c('PIECE', 'SLICED', 'SNACKING'))
ds <- ds[ -c(15:30) ]
str(ds)
ds <- ds[complete.cases(ds), ]

ds_small <- ds[, c(4, 7, 11)]
dummy_small_ds <- dummy_cols(ds_small)
X <- model.matrix( ~ .-1, ds_small)


Y <- ds$Total_Sales
fit <- glmnet(x = X, y = Y, alpha = 1)
plot(fit, xvar = "lambda")
coef(fit, s = 0.3)


dummy_ds <- dummy_cols(ds) 

ds.lm <- lm(Total_Sales ~ ., data = dummy_ds) 
summary(ds.lm)
vif(ds.lm)