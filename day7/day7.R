# day 7
library(tidyverse)
input <- read_lines("day7/input7.txt")
extract <- str_extract_all(input, "[A-Z]", simplify = T)[, 2:3] %>% # crude extract the capital letters, drop the S
  as_data_frame() %>%
  rename(First = V1, Second = V2) %>%
  mutate(starting = First %in% Second) %>% # find those that can start
  arrange(starting, First, Second)

# every step you have to identify possibles, then sort
# table of predicates
table(extract$Second, extract$First)

order_list <- vector("character", 26) # length of the alphabet.

# checks
# do you have any predicate?
# are all your predicates done?
# are you alphabetically first?

# test
test <- c(
  "Step C must be finished before step A can begin.",
  "Step C must be finished before step F can begin.",
  "Step A must be finished before step B can begin.",
  "Step A must be finished before step D can begin.",
  "Step B must be finished before step E can begin.",
  "Step D must be finished before step E can begin.",
  "Step F must be finished before step E can begin."
)

ext_test <- str_extract_all(test, "[A-Z]", simplify = T)[, 2:3] %>% # crude extract the capital letters, drop the S
  as_data_frame() %>%
  rename(First = V1, Second = V2) %>%
  mutate(starting = First %in% Second) %>% # find those that can start
  arrange(starting, First, Second)

table(ext_test$Second, ext_test$First)

order_list <- vector("character", 6)

# let's start with a little brute force
# first step
# check which  are not in the second column, i.e. they have no dependencies.
order_list[1] <- ext_test[!(ext_test$First %in% ext_test$Second), ] %>%
  pull(First) %>%
  unique() %>% # all possible unique first steps
  sort() %>%
  .[1] # pull the first alphabeticaly

# second step
# we need to move those with C in the first column over to the first column - only if they don't have other requiements
possible <- ext_test[ext_test$First %in% order_list, ] %>% pull(Second) %>% unique()
# test if they are in the second more than once, i.e. have more requirements...
if (nrow(ext_test[ext_test$Second %in% possible, ]) == length(possible)) {
  order_list[2] <- c(ext_test$First[!(ext_test$First %in% ext_test$Second)][ext_test$First %in% order_list], possible) %>%
    sort() %>%
    .[1]
}


## Okay how can I automate this...

order_steps <- function(df) {

  # Make an output vector
  order_list <- character(length(unique(c(df$First, df$Second))))

  # let's start with a little brute force
  # first step
  # check which  are not in the second column, i.e. they have no dependencies.
  first <- df[!(df$First %in% df$Second), ]$First %>% unique() # all possible unique first steps
  order_list[1] <- sort(first) %>% .[1] # pull the first alphabeticaly

  for (i in 2:length(order_list)) {
    potential <- c(
      filter(df, First %in% order_list, !(Second %in% order_list))$Second,
      first[!(first %in% order_list)]
    )

    # test if they are in the second more than once, i.e. have more requirements...
    incomplete <- filter(df, Second %in% potential, !(First %in% order_list))$Second
    possible <- potential[!(potential %in% incomplete)]
    order_list[i] <- sort(possible) %>% .[1]
  }
  return(glue::glue_collapse(order_list))
}

order <- "CABDFE"
testthat::test_that(order_steps(ext_test), order)

order_steps(extract)

## Puzzle two

# Everything takes time...
# 5 workers
# So now up to five jobs can start at once...

# B, K, V can all start
# will take
# 62, 71, and 82 seconds respectively. so the order is going to be all different.

available_steps <- function(df, complete = NA) {
  first <- df[!(df$First %in% df$Second), ]$First %>% unique()

  # given a vector of complete.. what are availble.. in order.
  if (any(is.na(complete))) {
    return(first) # all possible unique first steps)
  }

  potential <- c(
    filter(df, First %in% complete, !(Second %in% complete))$Second,
    first[!(first %in% complete)]
  )
  incomplete <- filter(df, Second %in% potential, !(First %in% complete))$Second
  possible <- potential[!(potential %in% incomplete)]
  return(sort(unique(possible)))
}


steps <- function(df, steps = 26) {
  tasks <- tibble(task = NA, worker = NA, starts = NA, finishes = NA)

  available_workers <- 1:5
  next_steps <- available_steps(df)
  complete <- NULL

  for (i in 1:steps) {
    # each step gets it's own row:

    while (length(next_steps) == 0) {
      active <- filter(tasks, worker %in% in_use, !(task %in% complete$task))
      complete <- rbind(complete, active[1, ])
      next_steps <- available_steps(df, complete = unlist(complete$task))
      next_steps <- next_steps[!(next_steps %in% tasks$task)]
      available_workers <- suppressMessages(c(
        complete$worker[nrow(complete)],
        available_workers
      ) %>%
        tibble(worker = .) %>%
        left_join(., tasks) %>%
        group_by(worker) %>%
        summarise(max = max(finishes)) %>%
        arrange(max) %>%
        .$worker)
      in_use <- in_use[!(in_use %in% available_workers)]
    }

    if (!is.null(complete)) {}
    tasks[i, "task"] <- next_steps[1]
    tasks[i, "worker"] <- available_workers[1]
    tasks[i, "starts"] <- ifelse(is.null(complete), 1,
      complete$finishes[nrow(complete)] + 1
    )
    tasks[i, "finishes"] <- ifelse(is.null(complete),
      60 + which(LETTERS == next_steps[1]),
      complete$finishes[nrow(complete)] + 60 + which(LETTERS == next_steps[1])
    )

    in_use <- tasks %>%
      mutate(last_start = max(starts, na.rm = T)) %>%
      group_by(worker) %>%
      summarise(in_use = max(finishes, na.rm = T) >= last_start[1]) %>%
      filter(in_use == T) %>%
      pull(worker)

    available_workers <- (1:5)[!((1:5) %in% in_use)]
    next_steps <- next_steps[-1]
  }
  return(tasks)
}
