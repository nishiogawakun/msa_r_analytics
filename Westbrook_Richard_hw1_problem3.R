## Stores the two sets of ratings as two separate factor strings
## Rounds any decimal to the nearest integer
Scout_1 <- factor(round(c(1, 2.5, 3, 3, 4, 3, 5, 4)), levels = c(1:5))
Scout_2 <- factor(round(c(2, 3, 3, 4, 4, 5, 5, 5)), levels = c(1:5))

## Identifies how many players that the two scouts agreed on with the same score
## Using R's default round to the nearest even settings
sum(Scout_1 == Scout_2)