#########  Day 3
#################

################
##data 
library(tidyverse)
raw <- read_lines("2024/Data/Day03.txt")

testing <- c("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))", 
             "mul(0,0)mul(x1, 02")

##################
# Instructions Part 1 
# It seems like the goal of the program is just to multiply some numbers. It
# does that with instructions like mul(X,Y), where X and Y are each 1-3 digit
# numbers. For instance, mul(44,46) multiplies 44 by 46 to get a result of 2024.
# However, because the program's memory has been corrupted, there are also many
# invalid characters that should be ignored, even if they look like part of a
# mul instruction. Sequences like mul(4*, mul(6,9!, ?(12,34), or mul ( 2 , 4 )
# do nothing. 
###################


## want to use regex to find valid string, extract number pairs, multiply.

# start with test data

find_pairs <- function(strings){
  pairs <- str_extract_all(strings, pattern = "mul\\([0-9]{1,3},[0-9]{1,3}\\)")
  data.frame(unlist(pairs)) %>%
    mutate(pairs = str_remove_all(unlist.pairs., pattern = "mul\\(|\\)")) %>%
    separate(pairs, into = c("first", "second"), sep = ",", convert = TRUE) %>%
    mutate(product = first*second) %>% select(product) %>%
    colSums()
}

#works
find_pairs(testing)  

#my data
find_pairs(raw)
#187833789 


##################
# Instructions Part 2
# The do() instruction enables future mul instructions.
# The don't() instruction disables future mul instructions.
# Only the most recent do() or don't() instruction applies. 
# At the beginning of the program, mul instructions are enabled.
####################


# more testing
newtest <- c("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

# need to have a cascading effect, probably more regex
fancy_pairs <- function(stringdat){
  
  # need the pairs but also the DO and DONT values
  pairs <- str_extract_all(stringdat, pattern = "(mul\\([0-9]{1,3},[0-9]{1,3}\\))|(don't\\(\\))|(do\\(\\))")
  #print(pairs)
  
  # name the list so we can group later
  pairs <- set_names(pairs, paste0("V", 1:length(pairs)))
  
  # get lists into a DF,  organize the group names from before
  data.frame(unlist(pairs)) %>%
    rownames_to_column() %>%
    transmute(rowname, 
              lineid = str_extract(rowname, pattern = "V[0-9]"),
              strings = unlist.pairs.) %>%

        #make a new column to track the do-ing-ness
    mutate(doit = ifelse(str_detect(strings, pattern = "do"), strings, NA)) %>% 
    #within in line, label each item as DO or DON't
    group_by(lineid) %>%
    fill(doit, .direction = "down") %>%
    replace_na(list(doit = "do()")) %>% 
  
    #keep only the DO mults
    filter(doit == "do()" & strings != "do()") %>% 
    ungroup() %>%
    #same as before
    mutate(pairs = str_remove_all(strings, pattern = "mul\\(|\\)")) %>%
    separate(pairs, into = c("first", "second"), sep = ",", convert = TRUE) %>%
    mutate(product = first*second) %>% select(product) %>%
    colSums()
}

fancy_pairs(newtest)
# test data works

fancy_pairs(raw)
# result: 
#97728793
# but not correct?  too high


#### OOHHH this is supposed to be one string, not multiple strings.  OK.
raw_combined <- paste(raw, collapse = "")
fancy_pairs(raw_combined)
## 94455185


### IT WORKS

