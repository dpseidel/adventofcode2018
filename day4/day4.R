## puzzle 1 day 4
library(tidyverse)
library(lubridate)
library(magrittr)
input <- read_lines("day4/input4")

record <- tibble(
  timestamp = lubridate::ymd_hm(str_sub(input, start = 2, end = 17)),
  guard = str_extract(input, "#\\d+"),
  action = str_split(input, "] ", simplify = T)[, 2]
) %>%
  arrange(timestamp) %>%
  fill(guard, .direction = "down")


record %>% mutate(diff = c(as.numeric(diff(timestamp)), NA)) %>% 
  filter(action == "falls asleep") %>% 
  group_by(guard) %>% 
  summarise(asleep = sum(diff)) %$% .[which.max(asleep),]

# what guard sleeps the most?
# guard 1217

# what minute does he spend asleep the most?
g1217 <- record %>% filter(guard == "#1217", action %in% c("falls asleep", "wakes up")) %>% 
  mutate(min = minute(timestamp), min_inc = ifelse(action == "wakes up", min - 1, min))

sleep <- which(g1217$action == "falls asleep")

sleeping <- list()
for (i in sleep) {
  sleeping[[i]] <- seq(from = as.numeric(g1217[i, "min_inc"]), to = as.numeric(g1217[i+1, "min_inc"])) 
}

sleeping %>% unlist() %>% table %>% which.max() %>% names
# 40

# your solution is your guard number times the minute they slept most
1217*40


## puzzle 2
# which guard is most frequently asleep at the same minute?

asleep_min <- function(guard_no){
  
  guard_rec <- record %>% filter(guard == guard_no, action %in% c("falls asleep", "wakes up")) %>% 
    mutate(min = minute(timestamp), min_inc = ifelse(action == "wakes up", min - 1, min))
  
  sleep <- which(guard_rec$action == "falls asleep")
  
  sleeping <- list()
  for (i in sleep) {
    sleeping[[i]] <- seq(from = as.numeric(guard_rec[i, "min_inc"]), to = as.numeric(guard_rec[i+1, "min_inc"])) 
  }
  
  sleeping %>% unlist() %>% table
  
}


guards <- record$guard %>% unique() %>% na.omit

map(guards, asleep_min) %>% map(., ~sort(.x, decreasing = T)[1]) %>% reduce(c) %>% which.max
# minute 34, guard[19] 

guards[19]
2789 *34


# kind of rushed at the end but a nice quick puzzle made easy with lubridate and tidyverse
