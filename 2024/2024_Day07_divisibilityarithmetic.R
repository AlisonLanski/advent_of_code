#########  Day 7
#################

################
##data 
library(tidyverse)
raw <- read_delim("2024/Data/Day07.txt", delim = "\\n", col_names = "maths") 

# OMG I'm making list-cols on purpose.  WHO AM I?
testing <- data.frame(maths = c("190: 10 19", 
                        "3267: 81 40 27", 
                        "83: 17 5", 
                        "156: 15 6", 
                        "7290: 6 8 6 15", 
                        "161011: 16 10 13", 
                        "192: 17 8 14", 
                        "21037: 9 7 18 13", 
                        "292: 11 6 16 20")) %>%
  separate(maths, into = c("result", "items"), sep = ": ") %>%
  mutate(items = str_extract_all(items, pattern = "[0-9]+")) %>%
  mutate(result = as.numeric(result), 
         items = map(items, as.numeric))
 
df <- testing
df <- raw


################## 
# Instructions Part 1 
#
# Each line represents a single equation. The test value appears before the
# colon on each line; it is your job to determine whether the remaining numbers
# can be combined with operators to produce the test value.
#
# Operators are always evaluated left-to-right, not according to precedence
# rules. Furthermore, numbers in the equations cannot be rearranged. Glancing
# into the jungle, you can see elephants holding two different types of
# operators: add (+) and multiply (*).
#
# The engineers just need the total calibration result, which is the sum of the
# test values from just the equations that could possibly be true.
# 
##################

# this works if you select 1 row at a time, but not for the whole DF at once.
### hmm
df %>% mutate(mult_possible = sum((result %% items[[1]]) == 0))

mult <- function(vect){
  product <- 1
  for (v in vect){
    product <- product*v
  }
  return(product)
}

modulo <- function(value, vect){
  mods <- list(1)
  for (v in vect){
    if(value %% v == 0){
      mods[[1]] <- c(mods[[1]], v)
    }
  }
  return(unlist(mods))
}

##### well look at a few other things
df_rules <- df %>% mutate(add_all = ifelse(map(items, sum) > result, 
                               "larger", 
                               ifelse(map(items, sum) == result,
                                      "perfect",
                                      "smaller")),
              mult_all = ifelse(map(items, mult) > result, 
                                "larger", 
                                ifelse(map(items, mult) == result,
                                       "perfect",
                                       "smaller"))) %>%
  mutate(perfect = (add_all == "perfect") | (mult_all == "perfect")) %>%
  mutate(possible = ifelse((mult_all == "smaller") & (add_all == "smaller"), 
                           FALSE,
                           ifelse(add_all == "larger", FALSE,
                                  TRUE))) %>%
  filter(possible == TRUE) %>%

# see if we are total products
  mutate(can_divide = map2(result, items, modulo)) %>%
  mutate(possible = ifelse(map(can_divide, length) == 1, FALSE, possible),
         factors = map2(map(items, length), 1, sum),
         perfect = ifelse(factors == unlist(map(can_divide, length)),
                          TRUE, 
                          perfect)) %>%
  filter(possible == TRUE) %>%
  mutate(mult_one = ifelse(!perfect & possible,
                           map(can_divide, tail, n=1),
                           1)) %>%
  #works up to here
    mutate(add_others = list(keep(items, function(x) x != mult_one)))
                           
## now, for the ones that do divide, need to start applying more arithmetic rules.

df
df_rules

unlist(map(df_rules$items, length))
length(df_rules$items[[1]])+1
length(df_rules$can_divide[[1]][[1]])

df_rules$items
df_rules$can_divide

################## 
# Instructions Part 2 
#
# 
##################

