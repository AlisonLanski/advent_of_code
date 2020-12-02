# --- Day 2: Password Philosophy ---
#   Your flight departs in a few days from the coastal airport; the easiest way down to the coast from here is via toboggan.
# 
# The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day. "Something's wrong with our computers; we can't log in!" You ask if you can take a look.
# 
# Their password database seems to be a little corrupted: some of the passwords wouldn't have been allowed by the Official Toboggan Corporate Policy that was in effect when they were chosen.
# 
# To try to debug the problem, they have created a list (your puzzle input) of passwords (according to the corrupted database) and the corporate policy when that password was set.
# 
# For example, suppose you have the following list:
# 
# 1-3 a: abcde
# 1-3 b: cdefg
# 2-9 c: ccccccccc
# Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.
# 
# In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.
# 
# How many passwords are valid according to their policies?


library(tidyverse)
dat <- read_delim("2020/Day02Data.txt", delim = "\n", col_names = "All")

head(dat)

#string manipulation, then application of the validity rule
dat %>% 
  #get the columns into their own spot and nice
  separate(All, into = c("Range", "Letter", "pw"), sep = " ") %>%
  separate(Range, into = c("mintimes", "maxtimes"), sep = "-") %>%
  mutate(Letter = str_remove(Letter, ":")) %>%
  mutate(mintimes = as.numeric(mintimes), maxtimes = as.numeric(maxtimes)) %>%
  #count the letters
  mutate(pwcount = str_count(pw, pattern = Letter)) %>%
  #compare the count to the rule
  mutate(pwok = pwcount >= mintimes & pwcount <= maxtimes) %>%
  #find out how many are ok
  count(pwok)



# 
# --- Part Two ---
#   While it appears you validated the passwords correctly, they don't seem to be what the Official Toboggan Corporate Authentication System is expecting.
# 
# The shopkeeper suddenly realizes that he just accidentally explained the password policy rules from his old job at the sled rental place down the street! The Official Toboggan Corporate Policy actually works a little differently.
# 
# Each policy actually describes two positions in the password, where 1 means the first character, 2 means the second character, and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of these positions must contain the given letter. Other occurrences of the letter are irrelevant for the purposes of policy enforcement.
# 
# Given the same example list from above:
# 
# 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
# 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
# 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
# How many passwords are valid according to the new interpretation of the policies?

#repeat the prep above
dat %>% 
  #get the columns into their own spot and nice
  separate(All, into = c("Range", "Letter", "pw"), sep = " ") %>%
  separate(Range, into = c("firstloc", "lastloc"), sep = "-") %>%
  mutate(Letter = str_remove(Letter, ":")) %>%
  mutate(firstloc = as.numeric(firstloc), lastloc = as.numeric(lastloc)) %>% 
  #pull out the letter at each location of interest
  mutate(firstpw = substring(pw, first = firstloc, last = firstloc),
         lastpw = substring(pw, first = lastloc, last = lastloc)) %>%
  #check the rule
  mutate(pwok = (firstpw != lastpw) & (firstpw == Letter | lastpw == Letter)) %>%
  count(pwok)


########################################

#Improvements:  do the processing once only

#get the data

library(tidyverse)
dat <- read_delim("2020/Day02Data.txt", delim = "\n", col_names = "All")

head(dat)

#prep the strings

#string manipulation, then application of the validity rule
dat %>% 
  #get the columns into their own spot and nice
  separate(All, into = c("Range", "Letter", "pw"), sep = " ") %>%
  separate(Range, into = c("range1", "range2"), sep = "-") %>%
  mutate(Letter = str_remove(Letter, ":")) %>%
  mutate(range1 = as.numeric(range1), range2 = as.numeric(range2)) %>%
  
  #prep for part 1: count the letters
  mutate(lettercount = str_count(pw, pattern = Letter)) %>%
  
  #prep for part 2: locate the letters
  mutate(firstloc = substring(pw, first = range1, last = range1),
         lastloc = substring(pw, first = range2, last = range2)) %>%
  
  #produce results for part 1: 
  mutate(pwok1 = lettercount >= range1 & lettercount <= range2) %>%
  
  #produce results for part 2:
  mutate(pwok2 = (firstloc != lastloc) & (firstloc == Letter | lastloc == Letter)) %>%
  
  #find out how many are ok
  #uncomment the one you want and comment the other one
  #count(pwok1)
  count(pwok2)



##########################
#could try to figure out the letters by using str... and then look at lists
#not figure out yet

locate_list <- str_locate_all(pattern = dat2$Letter, dat2$pw)
dat2[1,1] %in% locate_list[[1]][,1] + dat2[1,2] %in% locate_list[[1]][,1]
