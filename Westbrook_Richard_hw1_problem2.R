## Let item_a represent a random number string of 10 draws 
## and item_b represent a random number string of 10 draws. 
## For item_a, use a seed of 5 and for item_b, use a seed of 6.
set.seed(5)
item_a <- rnorm(n = 10, mean = 20, sd = 2)
set.seed(6)
item_b <- runif(n = 10, min = 15, max = 25)

## Round both item_a and item_b up to the next highest integer, 
## and use code to determine how many pairwise equal values 
## exist between item_a and item_b.
ceiling(item_a) == ceiling(item_b)