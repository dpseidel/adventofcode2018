# day 1

# puzzle 1
# test cases: 
# For example, if the device displays frequency changes of +1, -2, +3, +1, 
# then starting from a frequency of zero, the following changes would occur:
#   
# Current frequency  0, change of +1; resulting frequency  1.
# Current frequency  1, change of -2; resulting frequency -1.
# Current frequency -1, change of +3; resulting frequency  2.
# Current frequency  2, change of +1; resulting frequency  3.
# In this example, the resulting frequency is 3.
# 
# Here are other example situations:
#   
# +1, +1, +1 results in  3
# +1, +1, -2 results in  0
# -1, -2, -3 results in -6

# write function


# puzzle input
library(tidyverse)
input_vector <- read_table("day1/day1.1_input.txt", col_names = F) 

# puzzle 1
sum(input_vector)

# puzzle 2 -- cumsum, when does it become 
table2 <- input_vector %>% mutate(cumsum = cumsum(X1))

length(c(0, table2$cumsum))
c(0, table2$cumsum) %>% unique() %>% length()

# needs to loop the frequency changes... 
# write a while loop. 
# cleanest way ... most reproducible way is to write while loop
# fastest way in R is to just rep the input vector n times and cumsum, duplicate check

# takes over 100 reps to get any duplicates. 
x <- rep(table2$X1, 150)

# add the 0 to make sure you don't miss 0 replicated. 
c(0, cumsum(x))[which(duplicated(c(0,cumsum(x))))][1]

which(duplicated(c(0,cumsum(x))))[1]
# the 133729th obs!

# # write tests... ???
# library(testthat)
# test_that("frequency calibration is correct"){
# }


