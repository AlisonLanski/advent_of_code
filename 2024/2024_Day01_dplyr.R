#########  Day 1
#################

################
##data 
library(tidyverse)
raw <- read_delim("2024/Data/Day01.txt", col_names = FALSE, trim_ws = TRUE)
dat <- raw %>% transmute(First = X1, Second = X4)

##################
# Instructions Part 1
# pair up the numbers and measure how far apart they are. Pair up the smallest
# number in the left list with the smallest number in the right list, then the
# second-smallest left number with the second-smallest right number, and so on.
#
# Within each pair, figure out how far apart the two numbers are; you'll need to
# add up all of those distances.
###################
dat <- dat %>% mutate(original_index = row_number())

# sort each, subtract (abs for distance), then sum
part1 <- sum(abs(sort(dat$First)-sort(dat$Second)))
part1


##################
# Instructions Part 2
# This time, you'll need to figure out exactly how often each number from the
# left list appears in the right list. Calculate a total similarity score by
# adding up each number in the left list after multiplying it by the number of
# times that number appears in the right list.
####################

# prep lists with counts
Seconds <- count(dat, Second)
Firsts <- count(dat, First) #%>% filter(n > 1) # just checking -- no duplicates here

#combine and math
left_join(Firsts, Seconds, by = c("First" = "Second")) %>% 
  replace_na(list(n.y = 0)) %>% 
  mutate(Similarity = First*n.y) %>% select(Similarity) %>%
  colSums()
  
