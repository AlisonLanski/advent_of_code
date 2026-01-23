#########  Day 5
#################

################
##data 
library(tidyverse)
rawA <- read_delim("2024/Data/Day05A.txt", col_names = FALSE, delim = "|")
rawB <- read_lines("2024/Data/Day05B.txt")

testA <- data.frame(start = c("47|53",
                           "97|13",
                           "97|61",
                           "97|47",
                           "75|29",
                           "61|13",
                           "75|53",
                           "29|13",
                           "97|29",
                           "53|29",
                           "61|53",
                           "97|53",
                           "61|29",
                           "47|13",
                           "75|47",
                           "97|75",
                           "47|61",
                           "75|61",
                           "47|29",
                           "75|13",
                           "53|13")) %>%
  separate(col = "start", into = c("X1", "X2"), sep = "\\|")

testB <- c("75,47,61,53,29", 
                           "97,61,53,29,13", 
                           "75,29,13", 
                           "75,97,47,61,53", 
                           "61,13,29", 
                           "97,13,75,29,47")
################## 
# Instructions Part 1 
#
# The first section specifies the page ordering rules, one per line. The first
# rule, 47|53, means that if an update includes both page number 47 and page
# number 53, then page number 47 must be printed at some point before page
# number 53. 
#
# The second section specifies the page numbers of each update.  Each
# line is an update.
#
# To get the printers going as soon as possible, start by
# identifying which updates are already in the right order. 
#
# Determine which updates are already in the correct order. 
# What do you get if you add up the middle page number from those
# correctly-ordered updates?
##################
#testA_wider <- testA %>% group_by(X1) %>% summarize(afters = paste(X2, collapse = ", ")) %>% ungroup()
#as.integer(str_split(testA_wider$afters[2], pattern = ", ", simplify = TRUE))
#numlist <- testA %>%
#  count(X1)

dfA <- testA
dfA <- rawA

dfB <- testB
dfB <- rawB

Npossible <- 23

# this step works for test data but not for actual: 
# probably have a circle of numbers booooooooo
## BUT!!! It may work for a single number target at a time.  

numorder <- dfA %>%
  count(X2, sort = T, name = "ordering") %>%
  mutate(X2 = as.character(X2))

check_ready <- data.frame(dfB) %>%
  separate(dfB, into = paste0("Item",rep(1:Npossible)), sep = ",") %>%
  mutate(grouping = row_number()) %>%
  pivot_longer(-grouping, names_to = "item", values_to = "numbers") %>%
  filter(!is.na(numbers)) %>% 
  left_join(numorder, by = c("numbers" = "X2")) %>%
  replace_na(list(ordering = 0))

check_done <- check_ready %>%
  group_by(grouping) %>%
  mutate(problem = ordering > lead(ordering, default = 1000)) %>%
  summarize(problems = sum(problem)) %>%
  filter(problems == 0)

inner_join(check_ready, check_done, by = "grouping") %>%
  group_by(grouping) %>%
  mutate(index = as.numeric(str_extract(item, "[0-9]+$"))) %>%
  mutate(avgindex = mean(index)) %>%
  filter(avgindex == index) %>%
  mutate(numbers = as.numeric(numbers)) %>%
  select(numbers) %>%
  colSums()

#######################################

## let's make it work in a different way
dfA

#yeah the problem test is totally not working
dfB_prep <- data.frame(dfB) %>%
  separate(dfB, into = paste0("Item",rep(1:Npossible)), sep = ",") %>%
  mutate(grouping = row_number()) %>%
  pivot_longer(-grouping, names_to = "item", values_to = "numbers") %>%
  filter(!is.na(numbers)) %>%
  mutate(index = as.numeric(str_extract(item, "[0-9]+$"))) %>% 
  group_by(grouping) %>%
  mutate(indexavg = mean(index),
         nextnumber = lead(numbers, default = "100")) %>%
  ungroup()
#  mutate(problem = sum(lead(numbers) %in% dfA$X1[dfA$X2 == numbers])) #%>%
  # ungroup() %>%
  # count(problem)

dfA_wider <- dfA %>% group_by(X1) %>% arrange(X2) %>% summarize(afters = paste(X2, collapse = ", ")) %>% ungroup() %>%
  mutate(X1 = as.character(X1))

dfA_wider %>% filter(X1 == "61")

dfB_prep %>% 
  left_join(dfA_wider, by = c("numbers" = "X1")) %>% 
  mutate(problems = !str_detect(paste0(numbers, ", ", afters, ", 100"), pattern = nextnumber)) #%>%
  group_by(grouping) %>%
  summarize(problemcount = sum(problems)) %>%
  filter(problemcount == 0) %>%
  inner_join(dfB_prep) %>%
  filter(indexavg == index) %>% 
  transmute(numbers = as.numeric(numbers)) %>%
  colSums()



################## 
# Instructions Part 2 
# 
##################
