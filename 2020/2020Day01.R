#2020.1

#trying auto-read-in and running into auth problems
library(tidyverse)
library(xml2)
library(rvest)
library(curl)
xml2::read_html("https://adventofcode.com/2020/day/1/input")

con <- url("https://adventofcode.com/2020/day/1/input.txt", "rb")
??read_html
base::url("https://adventofcode.com/2020/day/1/input")

library(httr)

httr::GET("https://adventofcode.com/2020/day/1/input")

thing <- html_session("https://adventofcode.com/2020/day/1/input")


# --- Day 1: Report Repair ---
#   After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island. Surely, Christmas will go on without you.
# 
# The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.
# 
# To save your vacation, you need to get all fifty stars by December 25th.
# 
# Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
#   
#   Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.
# 
# Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
# 
# For example, suppose your expense report contained the following:
# 
# 1721
# 979
# 366
# 299
# 675
# 1456
# In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.
# 
# Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?
# 
# Your puzzle answer was 870331.
# 
# --- Part Two ---
# The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the same criteria.
# 
# Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.
# 
# In your expense report, what is the product of the three entries that sum to 2020?
# 
# Your puzzle answer was 283025088.



#############################
library(tidyverse)

#this works, but feels like copy-and-paste cheating
dat <- read_delim("2020/Day01Data.txt", delim = "\n", col_names = FALSE)
colnames(dat) <- "expense" 

head(dat)

#oh, for loops, I don't know how to quit you

#doing 1:199 and i+1:200 removes dup addition
for(i in 1:199){
  for(k in (i+1):200){
    if(dat$expense[k] + dat$expense[i] == 2020){
    print(paste0(dat$expense[k], "+", dat$expense[i], " = ", 
                 dat$expense[k]+dat$expense[i]))
      print(dat$expense[k]*dat$expense[i])
      print(i)
      print(k)
    }
  }
}

#something with a function and purrrrr?  but how?
adding <- function(i, j){
  dat$expense[i] + dat$expense[j]
}

adding(90, 151)


#challenge part 2 --- same thing, extra loop really should figure out purrrrrrrr
for(i in 1:198){
  for(j in (i+1):199){
    for(k in (i+2):200){
      if(dat$expense[k] + dat$expense[i] + dat$expense[j] == 2020){
        print(paste0(dat$expense[k], "+", dat$expense[i], "+", dat$expense[j]," = ", 
                     dat$expense[k]+dat$expense[i] +dat$expense[j]))
        print(dat$expense[k]*dat$expense[i]*dat$expense[j])
        print(i)
        print(j)
        print(k)
        stop("All done!")
        }
      }
    }
}

#################################################
#trying to speed it up 
#this is MUCH faster for the triple question
dat2 <- dat
dat3 <- dat

#part 1 again
for(i in 1:199){
    if(2020 %in% (dat$expense[i] + dat2$expense)){
      k <- which(dat2$expense == 2020-dat$expense[i])
      print(paste("indices:", i, k))    
      print(paste("values:", dat$expense[i], dat3$expense[k]))
      print(paste("product:", dat$expense[i]*dat3$expense[k]))
      stop("We found it")
    }
}

#part 2 again
for(i in 1:199){
  for(j in i+1:200){
    if(2020 %in% (dat$expense[i] + dat2$expense[j] +dat3$expense)){
      k <- which(dat3$expense == 2020-dat$expense[i]-dat2$expense[j])
      print(paste("indices:", i, j, k))    
      print(paste("values:", dat$expense[i], dat2$expense[j], dat3$expense[k]))
      print(paste("product:", dat$expense[i]*dat2$expense[j]*dat3$expense[k]))
      stop("We found it")
    }
  }
}

