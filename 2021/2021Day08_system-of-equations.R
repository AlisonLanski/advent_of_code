########
## Advent of code, 2021, Day 08

######
## Setup

#load
library(tidyverse)

runtype <- rstudioapi::showPrompt(title = "Type of run", message = "'test' or 'real'?")
day <- "08"

if(runtype == 'test'){
  filename <- paste0("2021Day", day, "test.txt")
} else {
  filename <- paste0("2021Day", day, ".txt")
}

df <- read_delim(file = here::here("2021", filename), col_names = FALSE, delim = ' | ', trim_ws = TRUE)
colnames(df) <-  c(paste0("signal", c(1:10)), "bar", paste0("result", c(1:4)))
df <- df %>% select(-bar)

rm(filename, day, runtype)


#########
### Context info
#normal system:
thesystem <-  data.frame(signal = c(0:9),
           components = c("abcefg",
                          "cf",
                          "acdeg",
                          "acdfg",
                          "bcdf",
                          "abdfg",
                          "abdefg",
                          "acf",
                          "abcdefg",
                          "abcdfg")) %>%
  mutate(compn = str_count(components))
thesystem %>% arrange(compn)

#1 is 2 letters
#7 is 3 letters
#4 is 4 letters
#8 is 7 letters
#2, 3, 5 are 5 letters
#0, 6, 9 are 6 letters

#########
### Working on Part 1
### In the output values, how many times do digits 1, 4, 7, or 8 appear?

#just have to count the number of unique 2, 3, 4, 8 letter words.  not hard.
# 
df %>% select(matches("result")) %>% 
  mutate(index = paste0("index", 1:nrow(.))) %>% #need something to pivot on
  pivot_longer(-index, names_to = 'ordering', values_to = 'outputs') %>%
  mutate(compn = str_count(outputs)) %>% 
  #get the unique string lengths
  filter(compn %in% c(2, 3, 4, 7)) %>% 
  count(compn) %>% 
  colSums()


##########
## Part 2 
## For each entry, determine all of the wire/segment
## connections and decode the four-digit output values. What do you get if you
## add up all of the output values?

# need to solve the system of equations
dat <- df[1,]
completeset <- df %>% 
  mutate(index = paste0("index", 1:nrow(.))) %>% #need something to pivot on
  pivot_longer(-index, names_to = 'ordering', values_to = 'codes') %>%
  mutate(compn = str_count(codes)) %>%
  #avoiding issues with using the same letters for everything
  mutate(codes = str_replace(codes, pattern = 'a', replacement = 't'),
         codes = str_replace(codes, pattern = 'b', replacement = 'u'),
         codes = str_replace(codes, pattern = 'c', replacement = 'v'),
         codes = str_replace(codes, pattern = 'd', replacement = 'w'),
         codes = str_replace(codes, pattern = 'e', replacement = 'x'),
         codes = str_replace(codes, pattern = 'f', replacement = 'y'),
         codes = str_replace(codes, pattern = 'g', replacement = 'z')
         ) %>%
  #tedious and probably a better way to do this, but puts the strings in order (I'm a fan of that)
  mutate(code2 = str_split(codes, pattern = '')) %>%
  mutate(code3 = map(.data$code2, str_sort)) %>%
  mutate(code4 = map(.data$code3, str_flatten)) %>%
  mutate(codes = unlist(code4)) %>%
  select(-code2, -code3, -code4)

#get one bit at a time
######## COULD LOOP THROUGH THIS WHOLE THING TO GET RESULTS
#### I bet there's a simpler way but.. not sure what it is
onerow <- completeset %>% filter(index == "index1") %>% 
  select(index, codes, compn) %>% distinct()

#this system will solve it for each letter
#2 letter 
cf <- str_split(onerow[onerow$compn == 2, 2], pattern = '') %>% unlist %>% paste(collapse = "|")

#compare 2-letter to 2-letter
a <- str_remove_all(onerow[onerow$compn == 3,2], pattern = cf)

#compare 2-letter to 4 letter
bd <- str_remove_all(onerow[onerow$compn == 4, 2], pattern = cf) %>% str_split(pattern = '') %>% unlist %>% paste(collapse = "|")

#compare 4-letter to singleton in 5-letters
b <- map(onerow[onerow$compn == 5, 2], str_split, pattern = '') %>% 
  unlist %>% data.frame(letters = .) %>% count(letters) %>% filter(n == 1) %>% select(letters) %>%
  mutate(findb = str_detect(letters, pattern = bd)) %>%
  filter(findb == TRUE) %>% select(letters) %>% as.character()

#finish 4-letter
d <- str_remove_all(bd, pattern = b) %>% str_remove_all(pattern = "\\|")

#get the 5-letter in-common
adg <- map(onerow[onerow$compn == 5, 2], str_split, pattern = '') %>% 
  unlist %>% data.frame(letters = .) %>% count(letters) %>% filter(n == 3) %>% select(letters) %>% 
  unlist %>% paste(collapse = "|") 

#remove the 5-letter in common to find the pair for b  
f <- map(onerow[onerow$compn == 5, 2], str_remove_all, pattern = paste(adg, b, sep = "|")) %>% 
  unlist %>% data.frame(letters = .) %>% mutate(counts = nchar(letters)) %>%
  filter(counts == 1) %>% select(letters) %>% as.character()

#clean up the 2-letter
c <- str_remove_all(cf, pattern = f) %>% str_remove_all(pattern = "\\|")

#clean up the five letter
g <- str_remove_all(adg, pattern = paste(a, d, "\\|", sep = "|"))

#last one
e <- str_remove_all('tuvwxyz', pattern = paste(a, b, c, d, f, g, sep = "|"))


#get the solved system key
systemset <- onerow %>% mutate(oldcodes = codes,
                  codes = str_replace_all(codes, pattern = a, replacement = 'a'),
                  codes = str_replace_all(codes, pattern = b, replacement = 'b'),
                  codes = str_replace_all(codes, pattern = c, replacement = 'c'),
                  codes = str_replace_all(codes, pattern = d, replacement = 'd'),
                  codes = str_replace_all(codes, pattern = e, replacement = 'e'),
                  codes = str_replace_all(codes, pattern = f, replacement = 'f'),
                  codes = str_replace_all(codes, pattern = g, replacement = 'g')) %>%
  mutate(code2 = str_split(codes, pattern = '')) %>%
  mutate(code3 = map(.data$code2, str_sort)) %>%
  mutate(code4 = map(.data$code3, str_flatten)) %>%
  mutate(codes = unlist(code4)) %>%
  select(-code2, -code3, -code4) %>% 
  left_join(thesystem, by = c("codes" = "components"))

#join back to original data and get the number out
completeset %>% filter(index == "index1") %>%
  left_join(systemset, by = c("codes" = "oldcodes")) %>% filter(str_detect(ordering, "result")) %>% select(signal) %>%
  unlist %>% paste(collapse = '') %>% as.numeric()
