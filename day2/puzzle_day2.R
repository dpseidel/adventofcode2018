# day2 puzzle 1

# To make sure you didn't miss any, you scan the likely candidate boxes again,
# counting the number that have an ID containing exactly two of any letter and then separately
# counting those with exactly three of any letter. You can multiply those two counts together
# to get a rudimentary checksum and compare it to what your device predicts.
#
# For example, if you see the following box IDs:
#
# abcdef contains no letters that appear exactly two or three times.
# bababc contains two a and three b, so it counts for both.
# abbcde contains two b, but no letter appears exactly three times.
# abcccd contains three c, but no letter appears exactly two times.
# aabcdd contains two a and two d, but it only counts once.
# abcdee contains two e.
# ababab contains three a and three b, but it only counts once.

library(tidyverse)
test <- c("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")

checksum <- function(x) {
  tab <- x %>%
    str_split(., "") %>%
    map_df(., function(x) {
      table(x) %>% as_data_frame(.) %>% group_by(n) %>% tally(wt = NULL)
    }, .id = "box") %>%
    filter(n %in% 2:3) %>%
    group_by(box, n) %>%
    tally(wt = NULL) %>%
    group_by(n) %>%
    tally(wt = NULL)

  prod(tab$nn)
}

library(testthat)
expect_equal(checksum(test), 12)

input <- read_table("day2/input_day2.txt", col_names = F)

checksum(input$X1)

## puzzle 2

# find the boxes that differ by only one number in the exact position

test2 <- c(
  "abcde",
  "fghij",
  "klmno",
  "pqrst",
  "fguij",
  "axcye",
  "wvxyz"
)

matching_letters <- function(vct) {
  test2 <- vct %>% str_split("", simplify = T)

  cross <- seq_len(length(vct)) %>%
    list(v1 = ., v2 = .) %>%
    cross_df(.filter = `==`) %>%
    as.matrix()

  test <- map_lgl(1:nrow(cross), function(i) {
    i1 <- cross[i, 1]
    i2 <- cross[i, 2]

    sum(!(test2[i1, ] == test2[i2, ])) == 1
  })

  match <- cbind(cross, test) %>% as_data_frame() %>% filter(test == 1)

  glue::glue_collapse(test2[as.numeric(match[1, 1]), ][test2[as.numeric(match[1, 1]), ] == test2[as.numeric(match[1, 2]), ]])

  # admittedly kind of hacky but it works... gross.
}

expect_equal(matching_letters(test2), "fgij")
matching_letters(input$X1)
