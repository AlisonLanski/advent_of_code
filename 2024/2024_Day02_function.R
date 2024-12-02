#########  Day 2
#################

################
##data 
library(tidyverse)
raw <- read_delim("2024/Data/Day02.txt", col_names = FALSE, trim_ws = TRUE)

#how many more columns?
max(str_count(raw$X5, " "))+1

#fix it
dat <- raw %>% separate(X5, 
                 into = c("X5", "X6", "X7", "X8"), 
                 sep =" ", 
                 remove = TRUE, 
                 convert = TRUE) %>%
  mutate(id = row_number())

##################
# Instructions Part 1 
# The engineers are trying to figure out which reports are
# safe. The Red-Nosed reactor safety systems can only tolerate levels that are
# either gradually increasing or gradually decreasing. So, a report only counts
# as safe if both of the following are true:
#
# The levels are either all increasing or all decreasing. 
# Any two adjacent levels differ by at least one and at most three.
###################

newdat <- data.frame(id = dat$id,
                     d1 = dat$X2-dat$X1,
                     d2 = dat$X3-dat$X2,
                     d3 = dat$X4-dat$X3,
                     d4 = dat$X5-dat$X4,
                     d5 = dat$X6-dat$X5,
                     d6 = dat$X7-dat$X6,
                     d7 = dat$X8-dat$X7) 


safeprep <- newdat %>% 
  pivot_longer(cols = -id, names_to = "d_order", values_to = "diff") %>%
  group_by(id) %>%
  
  #check 1-3 only
  mutate(stepsize = ifelse(max(abs(diff), na.rm = T) > 3, 
                           FALSE,
                           ifelse(min(abs(diff), na.rm = T) < 1, 
                                  FALSE,
                                  TRUE))) %>%
  #check increasing or decreasing
  mutate(direction = ifelse(sum(diff, na.rm = T)==(sum(abs(diff), na.rm=T)),
                            TRUE, 
                            ifelse(sum(abs(diff), na.rm = T)==-(sum(diff, na.rm = T)),
                                   TRUE,
                                   FALSE)))


# find winners, calc answer
safeprep %>%
  mutate(SAFE = stepsize & direction) %>%
  slice_max(order_by = d_order, n = 1) %>% 
  ungroup() %>% 
  select(SAFE) %>%
  colSums()

  


##################
# Instructions Part 2
# Now, the same rules apply as before, except if removing a single level from
# an unsafe report would make it safe, the report instead counts as safe.
####################

# start with the same prep
still_ok <- safeprep  %>%
  mutate(SAFE = stepsize & direction) %>%
  slice_max(order_by = d_order, n = 1) %>% 
  ungroup() %>%
  filter(SAFE)

# find the problems
maybe_ok <- safeprep  %>%
  mutate(SAFE = stepsize & direction) %>%
  filter(!SAFE)

# reprep the problems
testdat <- dat %>% filter(id %in% maybe_ok$id) %>%
  pivot_longer(cols = -id, names_to = "originals", values_to = "values") %>%
  group_by(id)


# set up df to hold fixed results
newsafe <- data.frame()

#recheck function
checkdf <- function(df){
  now_ok <- df %>%
    mutate(diff = lead(values) - values) %>%
    #check 1-3 only
    mutate(stepsize = ifelse(max(abs(diff), na.rm = T) > 3, 
                             FALSE,
                             ifelse(min(abs(diff), na.rm = T) < 1, 
                                    FALSE,
                                    TRUE))) %>%
    #check increasing or decreasing
    mutate(direction = ifelse(sum(diff, na.rm = T)==(sum(abs(diff), na.rm=T)),
                              TRUE, 
                              ifelse(sum(abs(diff), na.rm = T)==-(sum(diff, na.rm = T)),
                                     TRUE,
                                     FALSE))) %>%
  #### this is simpler for direction, I think 
  #       stepdirection = abs(lead(values)) > abs(values))  %>%
    mutate(SAFE = stepsize & direction) %>%
    filter(SAFE) %>%
    bind_rows(newsafe)
  return(now_ok)
}

# use recheck function with one column removed at a time
newsafe <- checkdf(testdat %>% filter(originals != "X1"))
newsafe <- checkdf(testdat %>% filter(originals != "X2"))
newsafe <- checkdf(testdat %>% filter(originals != "X3"))
newsafe <- checkdf(testdat %>% filter(originals != "X4"))
newsafe <- checkdf(testdat %>% filter(originals != "X5"))
newsafe <- checkdf(testdat %>% filter(originals != "X6"))  
newsafe <- checkdf(testdat %>% filter(originals != "X7"))
newsafe <- checkdf(testdat %>% filter(originals != "X8"))

new <- newsafe %>% select(id) %>% ungroup() %>% unique() %>% nrow()
old <- still_ok %>% select(id) %>% ungroup() %>% unique() %>% nrow()

new+old
