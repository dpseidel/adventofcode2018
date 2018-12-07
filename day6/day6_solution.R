# Using only the Manhattan distance, determine the area around each coordinate by
# counting the number of integer X,Y locations that are closest to that coordinate
# (and aren't tied in distance to any other coordinate).
#
# Your goal is to find the size of the largest area that isn't infinite.

library(tidyverse)
library(magrittr)

manhattan_dist <- function(p1, p2, q1, q2) {
  abs(p1 - q1) + abs(p2 - q2)
}


# test <- matrix(c(1, 1,
# 1, 6,
# 8, 3,
# 3, 4,
# 5, 5,
# 8, 9), ncol =2, byrow = T)
# 
# grid <- matrix(NA,10, 10)
# 
# for (x in 1:nrow(test)) {
#   grid[test[x, 2], test[x, 1]] <- letters[x]
# }
# 
# 
# for (i in 1:10) {
#   for (j in 1:10) {
#     dists <- manhattan_dist(i, j, test[,2], test[,1])
#     min_dist <- min(dists)
#     
#     if (length(dists[dists == min_dist]) != 1) {
#       grid[i, j] <- "."
#     } else {
#       grid[i, j] <- letters[which.min(manhattan_dist(i, j,  test[,2], test[,1]))]
#     }
#   }
# }
# 
# # the infinite touch the edges... 
# unique(c(grid[1,], grid[10,], grid[,1], grid[,10]))
# 
# area <- as.data.frame(table(grid))
# area[!(area$grid %in% unique(c(grid[1,], grid[10,], grid[,1], grid[,10]))),] %$% max(Freq)

# YAY it works. 


coords <- read_csv("day6/day6_coords.txt", col_names = F) %>%
  rename(col = X1, row = X2) %>%
  mutate(id = 1:nrow(.))


max(coords[, 1:2])
grid <- matrix(nrow = 400, ncol = 400)

for (i in 1:400) {
  for (j in 1:400) {
    dists <- manhattan_dist(i, j, coords$row, coords$col)
    min_dist <- min(dists)
    
    if (length(dists[dists == min_dist]) != 1) {
      grid[i, j] <- "."
    } else {
      grid[i, j] <- which.min(manhattan_dist(i, j, coords$row, coords$col))
    }
  }
}

# the infinite touch the edges... 
unique(c(grid[1,], grid[400,], grid[,1], grid[,400]))

area <- as.data.frame(table(grid))
area[!(area$grid %in% unique(c(grid[1,], grid[400,], grid[,1], grid[,400]))),] %$% max(Freq)
# YAY


# Puzzle 2
# On the other hand, if the coordinates are safe, maybe the best you can do is try to find a
# region near as many coordinates as possible.
# 
# For example, suppose you want the sum of the Manhattan distance to all of the coordinates 
# to be less than 32. For each location, add up the distances to all of the given coordinates; 
# if the total of those distances is less than 32, that location is within the desired region.

grid <- matrix(nrow = 400, ncol = 400)

for (i in 1:400) {
  for (j in 1:400) {
    dists <- manhattan_dist(i, j, coords$row, coords$col)
    sum_dist <- sum(dists)
    
    if (sum_dist < 10000) {
      grid[i, j] <- 1
    } else {
      grid[i, j] <- 0
    }
  }
}

sum(grid)

