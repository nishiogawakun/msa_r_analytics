## Stores two sets of character strings representing the 
## hyphenated order of cities as presented in the problem
Friend_A <- "Nashville-Boston-New York-Atlanta-Philadelphia-Tampa" 
Friend_B <- "Boston-Atlanta-Detroit-Chicago-New York-Orlando"

## Separates each city within the string by removing the hyphen
Friend_Ap <- strsplit(Friend_A, split = "-")
Friend_Bp <- strsplit(Friend_B, split = "-")

## Identifies which cities both friends have traveled to in the last year
intersect(unlist(Friend_Ap), unlist(Friend_Bp))

## Identifies which cities Friend A has gone to that Friend B has not in the last year
setdiff(unlist(Friend_Ap), unlist(Friend_Bp))
