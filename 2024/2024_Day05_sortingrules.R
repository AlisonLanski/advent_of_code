#########  Day 5
#################

################
##data 
library(tidyverse)
rawA <- read_delim("2024/Data/Day05A.txt", col_names = FALSE, delim = "|")
rawB <- read_lines("2024/Data/Day05B.txt")

testA <- data.frame(X1 = c(47,97,97,97,75,61,75,29,97,53,61,97,61,47,75,97,47,75,47,75,53),
               X2 = c(53,13,61,47,29,13,53,13,29,29,53,53,29,13,47,75,61,61,29,13,13))

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


### do we stop trying to build rules and just... check things?
# turn sequences in B into rule-pairs to check?
 
# control iteration
dfB_count <- length(dfB)

# track ok sets of numbers for each index
dfB_ok <- logical()

# set up loop:  for each vector in B, split into rule-pairs
# then check to see if all are accounted for in the full set of rule-pairs
for(C in 1:dfB_count){

  # order as vector, set up pair-holder
  dfBB <- as.integer(unlist(strsplit(dfB[C], ",")))
  dfBB_pairs <- data.frame()
  
  # produce all rule-pairs needed
  for(i in 1:length(dfBB)){
    lefty <- dfBB[i]
    if(i+1 <= length(dfBB)){
      righty <- dfBB[(i+1):(length(dfBB))]
      dfBB_pairs <- rbind(dfBB_pairs, expand.grid(lefty, righty))
    }
  }
  
  # colnames need to match for setdiff below
  dfBB_pairs <- dfBB_pairs %>%
    transmute(X1 = Var1,
              X2 = Var2)

  # if everything is in dfA, we should be good
  dfB_ok <- c(dfB_ok, nrow(setdiff(dfBB_pairs, dfA)) == 0)
}

# after loop:  find the ok vectors, find the middle numbers to sum 
dfB_find_middles <- dfB[dfB_ok]
middle_start <- nchar(dfB_find_middles)/2
middles <- substring(dfB_find_middles, first = middle_start, last = middle_start+1) %>% as.integer()
sum(middles)  ### WORKS!!!!!


################## 
# Instructions Part 2 
# 
##################


# While the Elves get to work printing the correctly-ordered updates, you have a little time to fix the rest of them.
# 
# For each of the incorrectly-ordered updates, use the page ordering rules to put the page numbers in the right order. 
# For the above example, here are the three incorrectly-ordered updates and their correct orderings:
#   
#  75,97,47,61,53 becomes 97,75,47,61,53.
#  61,13,29 becomes 61,29,13.
#  97,13,75,29,47 becomes 97,75,47,29,13.
#
#  After taking only the incorrectly-ordered updates and ordering them correctly, 
#  their middle page numbers are 47, 29, and 47. Adding these together produces 123.
# 
# Find the updates which are not in the correct order. 
# What do you get if you add up the middle page numbers after correctly ordering just those updates?
 

dfB_problems <- dfB[!dfB_ok]

#need to check and swap pairs
# copy loop code here and add prints to see what we need to do

# control iteration
dfB_count <- length(dfB_problems)

# place to store middle numbers
dfB_middle <- integer()

# set up loop:  for each vector in B, split into rule-pairs
# then check to see if all are accounted for in the full set of rule-pairs
for(C in 1:dfB_count){
  
  # order as vector, set up pair-holder
  dfBB <- as.integer(unlist(strsplit(dfB_problems[C], ",")))
  dfBB_pairs <- data.frame()
  
  # produce all rule-pairs needed
  for(i in 1:length(dfBB)){
    lefty <- dfBB[i]
    if(i+1 <= length(dfBB)){
      righty <- dfBB[(i+1):(length(dfBB))]
      dfBB_pairs <- rbind(dfBB_pairs, expand.grid(lefty, righty))
    }
  }
  
  
  # colnames need to match for setdiff below
  dfBB_pairs <- dfBB_pairs %>%
    transmute(X1 = Var1,
              X2 = Var2)
  
  # find the problems, swap them into successes
  flip <- setdiff(dfBB_pairs, dfA)
  colnames(flip) <- c("X2", "X1")
  flip <- flip %>% select(X1, X2)
  
  # combine fixed problems with original ones that were ok
  correct <- intersect(dfBB_pairs, dfA)
  rules <- rbind(correct, flip)
  rules_full <- rules
  
  # now run the rules-creating loop
  # this will work now that we don't have any circular rules 
  build_left <- integer()
  build_right <- integer()
  
  while(nrow(rules) > 0){
    # print(build_left)
    # print(build_right)
    
    # find distinct values on L and R sides
    only_left <- rules %>% distinct(X1) %>% rename(X = X1)
    only_right <- rules %>% distinct(X2) %>% rename(X = X2)
    
    # left but not right
    L <- setdiff(only_left, only_right) %>% pull()
    
    # right but not left
    R <- setdiff(only_right, only_left) %>% pull()
    
    # build patterns in order
    build_left <- c(build_left, L)
    build_right <- c(R, build_right)
    
    # reduce DF for identified rules
    rules <- rules %>% filter(X1 != L, X2 != R)
  }
  
  # find remaining value
  build_middle <- setdiff(rules_full %>% pull(), c(build_left, build_right)) 

  # to see the rule order: 
  #rule_order <- c(build_left, build_middle, build_right)
  
  # add the middle value for summing later
  dfB_middle <- c(dfB_middle, build_middle)

} 

# get answer -- works!
sum(dfB_middle)

  

