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