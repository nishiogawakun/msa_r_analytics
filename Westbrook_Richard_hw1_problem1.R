## If we assume that sides a and b are equal to 6 and 8 respectively, 
## construct the appropriate formula to generate the correct value for c.
a <- 6
b <- 8

## Let left_hand_side be an object equal to a2 + b2 
## and right_hand_side be an object equal to c2.
c2 <- a^2 + b^2
c <- sqrt(c2)

print(c)

## Show that left_hand_side and right_hand_side are equal.
a^2 + b^2 == c^2