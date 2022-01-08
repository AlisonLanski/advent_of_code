######
## Day 10, 2021

################

library(tidyverse)

day <- 10
run_type <- 'test'
run_type <- 'real'

if(run_type == 'test'){
  df <- read_lines(here::here(paste0("2021/2021Day", day, "test.txt")))
} else {
  df <- read_lines(here::here(paste0("2021/2021Day", day, ".txt")))
}

############
#### 
## Part 1
#### find mismatched symmetry in the strings

# remove pairs in sequence until all adjacent pairs are gone

df_less <- df
#just to make the condition work the first time through
#could probably write this in fewer lines with more "ors" but this is easier to read
df_less1 <- paste0(df,1)
i <- 1
while(sum(nchar(df_less) == nchar(df_less1)) < length(df_less)){ 
  df_less1 <- str_remove_all(df_less, pattern = "\\[\\]")
  df_less2 <- str_remove_all(df_less1, pattern = "<>")
  df_less3 <- str_remove_all(df_less2, pattern = "\\{\\}")
  df_less <- str_remove_all(df_less3, pattern = "\\(\\)")
  print(i)
  i <- i+1
}

#want to find a RL that don't match, assign points, sum
rl_issues <- "\\[\\}|\\[\\)|\\[>|\\{\\]|\\{\\)|\\{>|\\(\\]|\\(\\}|\\(>|<\\]|<\\}|<\\)"
#which strings have a problem
mismatches <- df_less[str_which(df_less, pattern = rl_issues)]
#which string is the first shown issue? what is the right side?
str_match(mismatches, pattern = rl_issues) %>% 
  substr(start = 2, stop = 2) %>% 
  data.frame(rights = .) %>% 
  mutate(points = case_when(rights == ']' ~ 57,
                              rights == '}' ~ 1197,
                              rights == ')' ~ 3,
                              rights == '>' ~ 25137)) %>%
  select(points) %>% colSums

# Your puzzle answer was 193275.
# 
# The first half of this puzzle is complete! It provides one gold star: *

##################
###### Part 2
## want to find the incompletes, complete them, points

## need to do the above, take the complement 
incompletes <- df_less[!str_detect(df_less, pattern = rl_issues)]

#prep for scores
#do a string reverse, then convert symbols to points
#then get the points into numeric vectors
completed_points <- stringi::stri_reverse(incompletes) %>% 
  str_replace_all(pattern = "\\[", replacement = "2") %>%
  str_replace_all(pattern = "\\{", replacement = "3") %>%
  str_replace_all(pattern = "\\(", replacement = "1") %>%
  str_replace_all(pattern = "<", replacement = "4") %>%
  str_split(pattern = '') %>% 
  modify(., as.numeric) 


#function to add up points for each vector
calcvalue <- function(vect){
  tot <- 0
  for(value in vect){
    tot <- tot*5 + value
  }
  return(tot)
}

#run it and sort from low to high
pointslist <- purrr::map(completed_points, calcvalue) %>% 
  unlist() %>% 
  sort()

#find the middle value
pointslist[ceiling(length(pointslist)/2)]

# Your puzzle answer was 2429644557. 
# Both parts of this puzzle are complete! They provide two gold stars: **