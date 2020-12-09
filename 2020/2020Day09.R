# # Day 9
# 
# --- Day 9: Encoding Error ---
#   With your neighbor happily enjoying their video game, you turn your attention to an open data port on the little screen in the seat in front of you.
# 
# Though the port is non-standard, you manage to connect it to your computer through the clever use of several paperclips. Upon connection, the port outputs a series of numbers (your puzzle input).
# 
# The data appears to be encrypted with the eXchange-Masking Addition System (XMAS) which, conveniently for you, is an old cypher with an important weakness.
# 
# XMAS starts by transmitting a preamble of 25 numbers. After that, each number you receive should be the sum of any two of the 25 immediately previous numbers. The two numbers will have different values, and there might be more than one such pair.
# 
# For example, suppose your preamble consists of the numbers 1 through 25 in a random order. 
# To be valid, the next number must be the sum of two of those numbers:
#   
#   26 would be a valid next number, as it could be 1 plus 25 (or many other pairs, like 2 and 24).
# 49 would be a valid next number, as it is the sum of 24 and 25.
# 100 would not be valid; no two of the previous 25 numbers sum to 100.
# 50 would also not be valid; although 25 appears in the previous 25 numbers, the two numbers in the pair must be different.
# Suppose the 26th number is 45, and the first number (no longer an option, as it is more than 25 numbers ago) was 20. 
# Now, for the next number to be valid, there needs to be some pair of numbers among 1-19, 21-25, or 45 that add up to it:
#   
#   26 would still be a valid next number, as 1 and 25 are still within the previous 25 numbers.
# 65 would not be valid, as no two of the available numbers sum to it.
# 64 and 66 would both be valid, as they are the result of 19+45 and 21+45 respectively.
# Here is a larger example which only considers the previous 5 numbers (and has a preamble of length 5):
#   
# 35
# 20
# 15
# 25
# 47
# 40
# 62
# 55
# 65
# 95
# 102
# 117
# 150
# 182
# 127
# 219
# 299
# 277
# 309
# 576
# In this example, after the 5-number preamble, almost every number is the sum of two of the previous 5 numbers; 
# the only number that does not follow this rule is 127.
# 
# The first step of attacking the weakness in the XMAS data is 
# to find the first number in the list (after the preamble) 
# which is not the sum of two of the 25 numbers before it. 
# What is the first number that does not have this property?

library(tidyverse)
testdat <- c(35,
   20,
   15,
   25,
   47,
   40,
   62,
   55,
   65,
   95,
   102,
   117,
   150,
  182,
  127,
  219,
  299,
  277,
  309,
  576)

testdat[1:5]
#want to find two additors in the previous numbers that sum to the next one
#can manage indices with incrementing
#have to avoid n*2 = n+1 which happens here with 20+20 = 40
#hmm if the situation works, there should be 2 numbers that match, and if 20+20 is in there, 
#that's just a third number.  So maybe we're okay if we limit by the number of TRUE results

n <- length(testdat)
p <- 1
q <- 5
for(i in 6:n){
  #print(testdat[i])
  result <- sum((testdat[i]-testdat[p:q]) %in% testdat[p:q]) >= 2
  #print(result)
  p <- p+1
  q <- q+1
  if(result == FALSE){
    stop(message = paste("No good for value", testdat[i], "as position", i))
  }
}


#okay run it for real on the main code
dat <- read_tsv("2020/Day09Data.txt", col_names = "nums")
#vector it
dat <- as.numeric(dat$nums) 

n <- length(dat)
p <- 1
q <- 25
for(i in 26:n){
  #print(dat[i])
  result <- sum((dat[i]-dat[p:q]) %in% dat[p:q]) >= 2
  #print(result)
  if(result == FALSE){
    stop(message = paste("No good for value", dat[i], "as position", i))
  } else{
    p <- p+1
    q <- q+1    
  }
}

#works

######################################################
# --- Part Two ---
#   The final step in breaking the XMAS encryption relies on the invalid number you just found: 
#   you must find a contiguous set of at least two numbers in your list which sum to the invalid number from step 1.
# 
# Again consider the above example:
#   
#   35
# 20
# 15
# 25
# 47
# 40
# 62
# 55
# 65
# 95
# 102
# 117
# 150
# 182
# 127
# 219
# 299
# 277
# 309
# 576
# In this list, adding up all of the numbers from 15 through 40 produces the invalid number from step 1, 127. 
# (Of course, the contiguous set of numbers in your actual list might be much longer.)
# 
# To find the encryption weakness, add together the smallest and largest number in this contiguous range; 
# in this example, these are 15 and 47, producing 62.
# 
# What is the encryption weakness in your XMAS-encrypted list of numbers?

#test it again
testsum <- 127
testsumloc <- which(testdat == 127)
i <- 1
j <- testsumloc-1
rdat <- testdat[i:j]
#do sums and chop off the ends one at a time

#this chops off the front and the back
#but the string could be 5 long near the front, so that doesn't help
#want to do this all the way through one side then all the way through the other side
while((sum(rdat) >= 127) & i < j){
  i <- i + 1
  #rdat <- rdat[i:testsumloc-i]  #somehow this actually does 1:(testsumloc-new-i) 
  rdat <- testdat[i:j]
  print(i)
  print(j)
  print(sum(rdat))
  print(rdat)
  print("iloop")
  for(k in i+1:j){
    j <- j - 1
    rdat <- testdat[k:j]
    print(i)
    print(k)
    print(j)
    print(sum(rdat))
    print(rdat)  
    print("jloop")
  }
#  reducedat <- testdat[i, testsuml]
}


#new method:
#different lengths of vector
#move the vector up


#this is working better
for(datlength in 3:10){
  endpoint <- testsumloc - datlength - 1
  i <- 1
  j <- datlength
  for(k in 0:endpoint){
    print(testdat[i:j+k])
    print(sum(testdat[i:j+k]))
    if(sum(testdat[i:j+k]) == 127){
      stop(message = "found it")
    }
  }  
}

#try on real data

sumloc <- 554  #from earlier code
sumvalue <- dat[554]

#picked 200 arbitrarily to keep loop time down, also bc the sums get pretty big pretty quickly
#can change range if it doesn't work (but it did)
for(datlength in 2:200){
  endpoint <- sumloc - datlength - 1
  i <- 1
  j <- datlength
  
  #okay what k does is moves the entire i:j forward (or back, if negative) one index
  for(k in 0:endpoint){
    print(dat[i:j+k])
    print(sum(dat[i:j+k]))
    if(sum(dat[i:j+k]) == sumvalue){
      #what we have to produce is the high+low out of the range
      stop(message = paste("found it: min plus max value =",
                              min(dat[i:j+k])+max(dat[i:j+k]), 
                              "and position of segment is ", k, ":", j+k))
    }
  }  
}


