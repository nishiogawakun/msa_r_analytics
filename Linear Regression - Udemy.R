library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)

df <- read.csv("student-mat.csv", sep = ';')

head(df)
summary(df)
any(is.na(df))
str(df)

num.col <- sapply(df, is.numeric)
cor.data <- cor(df[, num.col])
print(cor.data)

print(corrplot(cor.data, method = 'color'))

corrgram(df, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)

ggplot(df, aes(x = G3)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')

# Used for the lecture to follow along, unnecessary otherwise
set.seed(101)

sample <- sample.split(df$G3, SplitRatio = 0.7)

train <- subset(df, sample == TRUE)
test <- subset(df, sample == FALSE)

model <- lm(G3 ~ ., data = train)
summary(model)

res <- residuals(model)
class(res)

res <- as.data.frame(res)
head(res)

# Note: we want residuals to be normally distributed
ggplot(res, aes(res)) + geom_histogram(fill = 'blue', alpha = 0.5)


