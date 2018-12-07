# day 5

# puzzle 1
library(tidyverse)
input <- read_lines("day5/input5.txt")

collapse_polymers <- function(split) {
  repeat{
    t <- split %>% cbind(., c(.[-1], NA))
    eval <- t[, 1] != t[, 2] & (t[, 1] == tolower(t[, 2]) | t[, 1] == toupper(t[, 2]))

    if (all(eval == FALSE, na.rm = T)) {
      return(split)
    }

    drop <- which(eval)

    if (1 %in% diff(drop)) {
      drop <- drop[-which(c(NA, diff(drop) == 1))]
    }

    drop <- c(drop, drop + 1)

    split <- split[-drop]
  }
}

library(testthat)
test <- "dabAcCaCBAcCcaDA"
result <- "dabCBAcaDA"
str_count(result) == 10

test_that("polymers collapse correctly", {
  x <-  str_split(test, "")[[1]]
  y <- collapse_polymers(x)
  expect_equal(10, length(y))
  expect_equal(glue::glue_collapse(y), result)
  
})

split <- str_split(input, "")[[1]]
answer <- collapse_polymers(split)
length(answer) 


## puzzle 2
split <- str_split(input, "")[[1]]

remove_react <- function(split, polymer){
  trim <- split[-which(split %in% c(polymer, toupper(polymer)))]
  collapse_polymers(trim)
}

lens <- map_dbl(letters, ~length(remove_react(input, .x)))
min(lens)
