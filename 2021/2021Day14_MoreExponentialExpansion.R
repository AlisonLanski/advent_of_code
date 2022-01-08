######
## Day 14, 2021

################

library(tidyverse)

day <- 14
run_type <- rstudioapi::showPrompt("Run Type", "test or real?", "test")

if(run_type == 'test'){
  filepath <- here::here(paste0("2021/2021Day", day, "test.txt"))
  starter <- "NNCB"
} else {
  filepath <- here::here(paste0("2021/2021Day", day, ".txt"))
  starter <- "NNSOFOCNHBVVNOBSBHCB"
}

df <- read_delim(filepath, delim = " -> ", 
                 col_names = c("pairs", "newone"), col_types = "cc") %>%
  mutate(newgroupleft = paste0(substr(pairs, 1, 1), 
                           newone),
         newgroupright = paste0(newone, substr(pairs,2,2)))#, 


###########
### Part 1: get max and min after 10 runs, subtract

## set up inserter function 
# (we are actually making the strings)

inserting <- function(stringthing, df){
  letters <- character()
  for(i in 1:nchar(stringthing)){
    letters <- c(letters, substr(stringthing, i, i+1))
  }
  
  letters <- letters %>% data.frame(letters = .) %>% 
    left_join(df, by = c("letters" = "pairs")) %>%
    transmute(letters = case_when(is.na(newgroupleft) ~ letters,
                                  TRUE ~ newgroupleft)) %>% 
    pull(letters) %>% paste(collapse = '')
  
  return(letters)
}

nruns <- 10
starter_bruteforce <- starter
for(i in 1:nruns){
  print(paste0("Run: ", i))
  starter_bruteforce <- inserting(starter_bruteforce, df)
}

#get max and min counts, subtract
str_split(starter_bruteforce, '') %>% unlist %>% data.frame(items = .) %>% count(items) %>% arrange(n)

#subtract maxcount from mincount (10 runs)
4734-828

##############################
#############
### Part 2: If we use the inserter for 40 runs, that'll blow up the computer

# try new method: find what happens to each pair of transforms after 40 runs and go with that.
# in other words, count the starting letters, count the target pair letters, cycle through
# will also work for the 10-run solution

########
### Setup

#get the starter into individual letters (counted)
starter2 <- character()
for(i in 1:nchar(starter)){
  starter2 <- c(starter2, substr(starter, i, i))
}
starter2 <- starter2 %>% data.frame(newone = .) %>%
  count(newone)

#get the starter into pairs (counted)
starter_pairs <- character()
for(i in 1:nchar(starter)){
  starter_pairs <- c(starter_pairs, substr(starter, i, i+1))
}
starter_pairs <- starter_pairs %>%
  data.frame(pairs = .) %>%
  count(pairs, name = 'paircounter')

#function to increment letter-counts
count_more_letters  <- function(existing_letters, new_letters){
  existing_letters <- existing_letters %>%
    left_join(new_letters, by = c("letter_holder" = "newone")) %>%
    replace_na(list(n = 0)) %>%
    transmute(letter_holder,
              counter = counter+n)
  return(existing_letters)
}


#take current pairs and count the new ones
add_more_pairs <- function(starting_pairs, df){
  new_pairs <- starting_pairs %>%
  left_join(df, by = "pairs") %>%
  filter(!is.na(newone)) %>%
  select(-pairs) %>%
  pivot_longer(cols = c(newgroupleft, newgroupright), 
               names_to = 'pairlocation', 
               values_to = 'pairs') %>%
  group_by(pairs, newone) %>%
  summarize(paircounter = sum(paircounter)) %>%
  ungroup()
  return(new_pairs)
}

############
#### get going

#set up a df to count available letters
letter_counter <- df %>% select(newone) %>% distinct() %>% 
  transmute(letter_holder = newone, counter = 0)
#add the starter to the set of letters
letter_counter<- count_more_letters(letter_counter, starter2)

#iterate through and expand
nruns <- 40
new_pairs <- starter_pairs
for(i in 1:nruns){
  print(paste0("Run: ", i))
  pair_tracker <- add_more_pairs(starting_pairs = new_pairs, df = df)
  
  #this should go into add new pairs 
  new_pairs <- pair_tracker %>% select(-newone)
  
  #this goes into the letter-counter
  for_counting_letters <- pair_tracker %>% 
    group_by(newone) %>%
    #divide by 2 bc letters are counted in L and R side pairs (dups)
    summarize(n = sum(paircounter)/2) %>%
    ungroup()
  
  letter_counter <- count_more_letters(existing_letters = letter_counter, 
                                       new_letters = for_counting_letters)

}

#when done, check results; pull out biggest/smallest and subtract
letter_counter %>% arrange(counter)
options(scipen = 999)
5246955808499 - 805638546047
