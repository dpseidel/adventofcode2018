# day 3 puzzle 1

# The whole piece of fabric they're working on is a very large square - at least 1000 inches on each side.
#
# Each Elf has made a claim about which area of fabric would be ideal for Santa's suit.
# All claims have an ID and consist of a single rectangle with edges parallel to the edges of the fabric.
# Each claim's rectangle is defined as follows:
#
# The number of inches between the left edge of the fabric and the left edge of the rectangle.
# The number of inches between the top edge of the fabric and the top edge of the rectangle.
# The width of the rectangle in inches.
# The height of the rectangle in inches.
# A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge,
# 2 inches from the top edge, 5 inches wide, and 4 inches tall. Visually, it claims the square inches of fabric
# represented by # (and ignores the square inches of fabric represented by .) in the diagram below:
#
# ...........
# ...........
# ...#####...
# ...#####...
# ...#####...
# ...#####...
# ...........
# ...........
# ...........
# The problem is that many of the claims overlap, causing two or more claims to cover part of the same areas.
# For example, consider the following claims:
#
# #1 @ 1,3: 4x4
# #2 @ 3,1: 4x4
# #3 @ 5,5: 2x2
# Visually, these claim the following areas:
#
# ........
# ...2222.
# ...2222.
# .11XX22.
# .11XX22.
# .111133.
# .111133.
# ........
# The four square inches marked with X are claimed by both 1 and 2.
# (Claim 3, while adjacent to the others, does not overlap either of them.)
#
# If the Elves all proceed with their own plans, none of them will have enough fabric.
# How many square inches of fabric are within two or more claims?

library(tidyverse)
# library(sf)

input <- read_delim("day3/puzzle3input.txt", col_names = F, delim = " ") %>%
  mutate(
    no. = str_split(X1, "#", simplify = T)[, 2],
    from_left = str_split(X3, ",", simplify = T)[, 1],
    from_top = str_split(str_split(X3, ",", simplify = T)[, 2], ":", simplify = T)[, 1], # gross but functional dana
    width = str_split(X4, "x", simplify = T)[, 1],
    height = str_split(X4, "x", simplify = T)[, 2]
  ) %>%
  select(5:9) %>%
  mutate_all(as.numeric)

summary(input)

test <- tribble(
  ~no., ~from_left, ~from_top, ~width, ~height,
  1, 1, 3, 4, 4,
  2, 3, 1, 4, 4,
  3, 5, 5, 2, 2
)


# patch_poly <- function(row) {
#   patch <- unlist(row[2:5])
#
#   coords <- list(matrix(c(
#     patch["x"], patch["y"],
#     patch["x"] + patch["width"], patch["y"],
#     patch["x"] + patch["width"], patch["y"] + patch["height"],
#     patch["x"], patch["y"] + patch["height"],
#     patch["x"], patch["y"]
#   ),
#   ncol = 2, byrow = TRUE
#   ))
#   st_polygon(coords)
# }
#
#
# patch_poly(test[1, ])
#
# map(1:nrow(test),~patch_poly(test[.x,])) %>% st_sfc %>% st_intersection()

## This is probably not the best way to go because we need how many sqaure inches are within two claims ...
## now, is that "exactly" two claims? or just writ large how mnay are in dispute?
## i think calculating the total square inches in dispute via polygon area may inflate my value by duplicating inches..

# let's try a matrix apprach

fabric <- matrix(NA, 1000, 1000)

for (i in input$no.) {
  patch <- unlist(input[i, ])

  cols <- (patch["from_left"] + 1):(patch["from_left"] + patch["width"])
  rows <- (patch["from_top"] + 1):(patch["from_top"] + patch["height"])
  fabric[rows, cols] <- ifelse(is.na(fabric[rows, cols]), patch["no."], -999)
}

sum(fabric == -999, na.rm = T)

# that was nice and simple.

# puzzle 2
# what's the id of the one claim that doesn't overlap!
# rerun this loop but if the patch doesn't completely fit.. replace with NA


#### testing
test_fab <- matrix(NA, 8, 8)


for (i in test$no.) {
  patch <- unlist(test[i, ])

  cols <- (patch["from_left"] + 1):(patch["from_left"] + patch["width"])
  rows <- (patch["from_top"] + 1):(patch["from_top"] + patch["height"])
  test_fab[rows, cols] <- ifelse(is.na(test_fab[rows, cols]), patch["no."], -999)
}

# looks good
# now... to pull out 3

for (i in test$no.) {
  patch <- unlist(test[i, ])

  cols <- (patch["from_left"] + 1):(patch["from_left"] + patch["width"])
  rows <- (patch["from_top"] + 1):(patch["from_top"] + patch["height"])
  test_fab[rows, cols] <- ifelse(all(test_fab[rows, cols] == patch["no."]), patch["no."], -999)
}

test_fab %>% as.vector() %>% unique()
## looks good.
## hmm doesn't seem to work with the real input... and i don't know why.

### let's try a simpler value check.
# tidyverse wayyyy

areas <- input %>% mutate(area = width * height) %>% select(no., area)

# using `fabric` after first for loop
unoverlaps <- table(fabric) %>%
  as.tibble() %>%
  filter(fabric != "-999") %>%
  mutate_if(is.character, as.numeric)

left_join(unoverlaps, areas, by = c("fabric" = "no.")) %>%
  mutate(match = n == area) %>%
  filter(match == T)
