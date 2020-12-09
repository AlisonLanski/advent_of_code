# #Day 7 2020
# 
# --- Day 7: Handy Haversacks ---
#   You land at the regional airport in time for your next flight. In fact, it looks like you'll even have time to grab some food: all flights are currently delayed due to issues in luggage processing.
# 
# Due to recent aviation regulations, many rules (your puzzle input) are being enforced about bags and their contents; bags must be color-coded and must contain specific quantities of other color-coded bags. Apparently, nobody responsible for these regulations considered how long they would take to enforce!
# 
# For example, consider the following rules:
# 
# light red bags contain 1 bright white bag, 2 muted yellow bags.
# dark orange bags contain 3 bright white bags, 4 muted yellow bags.
# bright white bags contain 1 shiny gold bag.
# muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
# shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
# dark olive bags contain 3 faded blue bags, 4 dotted black bags.
# vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
# faded blue bags contain no other bags.
# dotted black bags contain no other bags.
# These rules specify the required contents for 9 bag types. In this example, every faded blue bag is empty, every vibrant plum bag contains 11 bags (5 faded blue and 6 dotted black), and so on.
# 
# You have a shiny gold bag. If you wanted to carry it in at least one other bag, how many different bag colors would be valid for the outermost bag? (In other words: how many colors can, eventually, contain at least one shiny gold bag?)
# 
# In the above rules, the following options would be available to you:
# 
# A bright white bag, which can hold your shiny gold bag directly.
# A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
# A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
# A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
# So, in this example, the number of bag colors that can eventually contain at least one shiny gold bag is 4.
# 
# How many bag colors can eventually contain at least one shiny gold bag? (The list of rules is quite long; make sure you get all of it.)


library(tidyverse)

#let's brute-force the sample
dat <- data.frame(rules = c("light red bags contain 1 bright white bag, 2 muted yellow bags",
"dark orange bags contain 3 bright white bags, 4 muted yellow bags",
"bright white bags contain 1 shiny gold bag",
"muted yellow bags contain 2 shiny gold bags, 9 faded blue bags", 
"shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags",
"dark olive bags contain 3 faded blue bags, 4 dotted black bags",
"vibrant plum bags contain 5 faded blue bags, 6 dotted black bags"))


#which ones contain shiny gold?
dat %>% filter(str_detect(rules, pattern = 'shiny gold'))

#these do
holderbags <- c("bright white", "muted yellow")

#ready for next step
holderbagsregex <- paste(holderbags, collapse = "|")
holderbagsstart <- paste0("^", paste(holderbags, collapse = "|^"))

#look again, one level up
dat %>% filter(str_detect(rules, pattern = holderbagsregex)) %>%
  filter(!str_detect(rules, pattern = holderbagsstart))

#ready for next step
holderbags2 <- c("light red", "dark orange")
holderbagsregex2 <- paste(holderbags2, collapse = "|")
holderbagsstart2 <- paste0("^", paste(holderbags2, collapse = "|^"))

#look again, two levels up
dat %>% filter(str_detect(rules, pattern = holderbagsregex2)) %>%
  filter(!str_detect(rules, pattern = holderbagsstart2))

#no more rows so this will show the number of "container bags" which should = the number of rules 
length(unique(c(holderbags, holderbags2, "bright white")))

#result = 4 which is correct

##########

#if you do this on all the bags, it'll be a LOT of rounds

dat <- read_tsv("2020/Day07Data.txt", col_names = "rules")

#e.g.
#which ones contain shiny gold?
bags <- "shiny gold"
holderbagsregex <- paste(bags, collapse = "|")
holderbagsstart <- paste0("^", paste(bags, collapse = "|^"))

bags2 <- dat %>% 
  filter(str_detect(rules, pattern = holderbagsregex)) %>%
  filter(!str_detect(rules, pattern = holderbagsstart)) %>%
  mutate(wherecontain = str_locate(rules, pattern = 'contain')) %>% 
  mutate(rules2 = substr(rules, start = 0, stop = wherecontain)) %>% 
  select(-wherecontain) %>%
  mutate(rules2 = str_remove(rules2, pattern = ' bags c$'))
  

#ready for next step
holderbagsregex <- paste(bags2$rules2, collapse = "|")
holderbagsstart <- paste0("^", paste(bags2$rules2, collapse = "|^"))

bags3 <- dat %>% 
  filter(str_detect(rules, pattern = holderbagsregex)) %>%
  filter(!str_detect(rules, pattern = holderbagsstart)) %>%
  mutate(wherecontain = str_locate(rules, pattern = 'contain')) %>% 
  mutate(rules2 = substr(rules, start = 0, stop = wherecontain)) %>% 
  select(-wherecontain) %>%
  mutate(rules2 = str_remove(rules2, pattern = ' bags c$'))



#ready for next step
holderbagsregex <- paste(bags3$rules2, collapse = "|")
holderbagsstart <- paste0("^", paste(bags3$rules2, collapse = "|^"))

bags4 <- dat %>% 
  filter(str_detect(rules, pattern = holderbagsregex)) %>%
  filter(!str_detect(rules, pattern = holderbagsstart)) %>%
  mutate(wherecontain = str_locate(rules, pattern = 'contain')) %>% 
  mutate(rules2 = substr(rules, start = 0, stop = wherecontain)) %>% 
  select(-wherecontain) %>%
  mutate(rules2 = str_remove(rules2, pattern = ' bags c$'))


#ready for next step
holderbagsregex <- paste(bags4$rules2, collapse = "|")
holderbagsstart <- paste0("^", paste(bags4$rules2, collapse = "|^"))

bags5 <- dat %>% 
  filter(str_detect(rules, pattern = holderbagsregex)) %>%
  filter(!str_detect(rules, pattern = holderbagsstart)) %>%
  mutate(wherecontain = str_locate(rules, pattern = 'contain')) %>% 
  mutate(rules2 = substr(rules, start = 0, stop = wherecontain)) %>% 
  select(-wherecontain) %>%
  mutate(rules2 = str_remove(rules2, pattern = ' bags c$'))



#ready for next step
holderbagsregex <- paste(bags5$rules2, collapse = "|")
holderbagsstart <- paste0("^", paste(bags5$rules2, collapse = "|^"))

bags6 <- dat %>% 
  filter(str_detect(rules, pattern = holderbagsregex)) %>%
  filter(!str_detect(rules, pattern = holderbagsstart)) %>%
  mutate(wherecontain = str_locate(rules, pattern = 'contain')) %>% 
  mutate(rules2 = substr(rules, start = 0, stop = wherecontain)) %>% 
  select(-wherecontain) %>%
  mutate(rules2 = str_remove(rules2, pattern = ' bags c$'))



#ready for next step
holderbagsregex <- paste(bags6$rules2, collapse = "|")
holderbagsstart <- paste0("^", paste(bags6$rules2, collapse = "|^"))

bags7 <- dat %>% 
  filter(str_detect(rules, pattern = holderbagsregex)) %>%
  filter(!str_detect(rules, pattern = holderbagsstart)) %>%
  mutate(wherecontain = str_locate(rules, pattern = 'contain')) %>% 
  mutate(rules2 = substr(rules, start = 0, stop = wherecontain)) %>% 
  select(-wherecontain) %>%
  mutate(rules2 = str_remove(rules2, pattern = ' bags c$'))


#ready for next step
holderbagsregex <- paste(bags7$rules2, collapse = "|")
holderbagsstart <- paste0("^", paste(bags7$rules2, collapse = "|^"))

bags8 <- dat %>% 
  filter(str_detect(rules, pattern = holderbagsregex)) %>%
  filter(!str_detect(rules, pattern = holderbagsstart)) %>%
  mutate(wherecontain = str_locate(rules, pattern = 'contain')) %>% 
  mutate(rules2 = substr(rules, start = 0, stop = wherecontain)) %>% 
  select(-wherecontain) %>%
  mutate(rules2 = str_remove(rules2, pattern = ' bags c$'))


#ready for next step
holderbagsregex <- paste(bags8$rules2, collapse = "|")
holderbagsstart <- paste0("^", paste(bags8$rules2, collapse = "|^"))

bags9 <- dat %>% 
  filter(str_detect(rules, pattern = holderbagsregex)) %>%
  filter(!str_detect(rules, pattern = holderbagsstart)) %>%
  mutate(wherecontain = str_locate(rules, pattern = 'contain')) %>% 
  mutate(rules2 = substr(rules, start = 0, stop = wherecontain)) %>% 
  select(-wherecontain) %>%
  mutate(rules2 = str_remove(rules2, pattern = ' bags c$'))


#ready for next step
holderbagsregex <- paste(bags9$rules2, collapse = "|")
holderbagsstart <- paste0("^", paste(bags9$rules2, collapse = "|^"))

bags10 <- dat %>% 
  filter(str_detect(rules, pattern = holderbagsregex)) %>%
  filter(!str_detect(rules, pattern = holderbagsstart)) %>%
  mutate(wherecontain = str_locate(rules, pattern = 'contain')) %>% 
  mutate(rules2 = substr(rules, start = 0, stop = wherecontain)) %>% 
  select(-wherecontain) %>%
  mutate(rules2 = str_remove(rules2, pattern = ' bags c$'))


#ready for next step
holderbagsregex <- paste(bags10$rules2, collapse = "|")
holderbagsstart <- paste0("^", paste(bags10$rules2, collapse = "|^"))

bags11 <- dat %>% 
  filter(str_detect(rules, pattern = holderbagsregex)) %>%
  filter(!str_detect(rules, pattern = holderbagsstart)) %>%
  mutate(wherecontain = str_locate(rules, pattern = 'contain')) %>% 
  mutate(rules2 = substr(rules, start = 0, stop = wherecontain)) %>% 
  select(-wherecontain) %>%
  mutate(rules2 = str_remove(rules2, pattern = ' bags c$'))

length(unique(c(bags2$rules2, 
                bags3$rules2, 
                bags4$rules2, 
                bags5$rules2, 
                bags6$rules2,
                bags7$rules2,
                bags8$rules2,
                bags9$rules2,
                bags10$rules2,
                bags11$rules2)))  #same as bags10
  



#################################################################3
#Part 2
# 
# --- Part Two ---
#   It's getting pretty expensive to fly these days - not because of ticket prices, but because of the ridiculous number of bags you need to buy!
# 
# Consider again your shiny gold bag and the rules from the above example:
# 
# faded blue bags contain 0 other bags.
# dotted black bags contain 0 other bags.
# vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted black bags.
# dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black bags.
# So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags within it) plus 2 vibrant plum bags (and the 11 bags within each of those): 1 + 1*7 + 2 + 2*11 = 32 bags!
# 
# Of course, the actual rules have a small chance of going several levels deeper than this example; be sure to count all of the bags, even if the nesting becomes topologically impractical!
# 
# Here's another example:
#   
#   shiny gold bags contain 2 dark red bags.
# dark red bags contain 2 dark orange bags.
# dark orange bags contain 2 dark yellow bags.
# dark yellow bags contain 2 dark green bags.
# dark green bags contain 2 dark blue bags.
# dark blue bags contain 2 dark violet bags.
# dark violet bags contain no other bags.
# In this example, a single shiny gold bag must contain 126 other bags.
# 
# How many individual bags are required inside your single shiny gold bag?
