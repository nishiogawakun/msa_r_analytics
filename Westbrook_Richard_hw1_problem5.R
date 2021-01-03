a <- "7" 
b <- "10" 
c <- "2017"

## Write a script that converts a, b, and c to "7/10/2017" 
## and stores it as an object called delivery_date.
delivery_date <- paste(a, b, c, sep = '/')

## Convert delivery_date into a date.
delivery_date <- as.Date(delivery_date, format = "%m/%d/%Y")

## Assume that 7/16/2017 is the delivery deadline. 
delivery_deadline <- as.Date("7/16/2017", format = "%m/%d/%Y")

## Show in R the difference between the delivery_deadline and the delivery_date.
print(delivery_deadline - delivery_date)

