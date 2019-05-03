## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(workout03)

## ------------------------------------------------------------------------
bin_choose( 5, 2 )== 10

## ------------------------------------------------------------------------
# probability of getting 2 successes in 5 trials
# (assuming prob of success = 0.5) 
bin_probability( 2, 5, 0.5)

