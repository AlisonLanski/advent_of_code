# #Day 5
# --- Day 5: Binary Boarding ---
#   You board your plane only to discover a new problem: you dropped your boarding pass! You aren't sure which seat is yours, and all of the flight attendants are busy with the flood of people that suddenly made it through passport control.
# 
# You write a quick program to use your phone's camera to scan all of the nearby boarding passes (your puzzle input); perhaps you can find your seat through process of elimination.
# 
# Instead of zones or groups, this airline uses binary space partitioning to seat people. A seat might be specified like FBFBBFFRLR, where F means "front", B means "back", L means "left", and R means "right".
# 
# The first 7 characters will either be F or B; these specify exactly one of the 128 rows on the plane (numbered 0 through 127). Each letter tells you which half of a region the given seat is in. Start with the whole list of rows; the first letter indicates whether the seat is in the front (0 through 63) or the back (64 through 127). The next letter indicates which half of that region the seat is in, and so on until you're left with exactly one row.
# 
# For example, consider just the first seven characters of FBFBBFFRLR:
# 
# Start by considering the whole range, rows 0 through 127.
# F means to take the lower half, keeping rows 0 through 63.
# B means to take the upper half, keeping rows 32 through 63.
# F means to take the lower half, keeping rows 32 through 47.
# B means to take the upper half, keeping rows 40 through 47.
# B keeps rows 44 through 47.
# F keeps rows 44 through 45.
# The final F keeps the lower of the two, row 44.
# The last three characters will be either L or R; these specify exactly one of the 8 columns of seats on the plane 
# (numbered 0 through 7). The same process as above proceeds again, this time with only three steps. 
# L means to keep the lower half, while R means to keep the upper half.
# 
# For example, consider just the last 3 characters of FBFBBFFRLR:
# 
# Start by considering the whole range, columns 0 through 7.
# R means to take the upper half, keeping columns 4 through 7.
# L means to take the lower half, keeping columns 4 through 5.
# The final R keeps the upper of the two, column 5.
# So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.
# 
# Every seat also has a unique seat ID: multiply the row by 8, then add the column. In this example, the seat has ID 44 * 8 + 5 = 357.
# 
# Here are some other boarding passes:
# 
# BFFFBBFRRR: row 70, column 7, seat ID 567.
# FFFBBBFRRR: row 14, column 7, seat ID 119.
# BBFFBBFRLL: row 102, column 4, seat ID 820.
# As a sanity check, look through your list of boarding passes. What is the highest seat ID on a boarding pass?

### here's what I wanted to do: create an actual binary tree and then navigate around on it nicely.  
## there are packages like data.tree that might be worth looking at.  dedrogram (baser) and rpart/party could help too maybe
## spent a little while looking at this online and decided the learning curve was a little steep
## let's tidyverse it instead



#set limits
rowmin <- 0
rowmax <- 127
seatmin <- 0
seatmax <- 7

#establish endpoints and Ns
#did this generally in anticipation of part 2
#turns out part 2 was about something else (oh well)
rows <- rep(rowmin:rowmax)
seats <- rep(seatmin:seatmax)
rr <- length(rows)
ss <- length(seats)

#set up DF trees
rowdf <- data.frame(last128 = rep(c(rep('F', 64), rep('B', 64)), rr/128), 
                    last64 = rep(c(rep('F', 32), rep('B', 32)), rr/64), 
                    last32 = rep(c(rep('F', 16), rep('B', 16)), rr/32), 
                    last16 = rep(c(rep('F', 8), rep('B', 8)), rr/16), 
                    last8 = rep(c(rep('F', 4), rep('B', 4)), rr/8), 
                    last4 = rep(c(rep('F', 2), rep('B', 2)), rr/4), 
                    last2 = rep(c('F', 'B'), rr/2), 
                    yourrow = rows)

seatdf <- data.frame(last8 = rep(c(rep('L', 4), rep('R', 4)), ss/8),
                     last4 = rep(c(rep('L', 2), rep('R', 2)), ss/4),
                     last2 = rep(c('L', 'R'), ss/2),
                     yourseat = seats)

#cleanup
rm(rowmin, rowmax, seatmin, seatmax, rows, seats, rr, ss)

# #TEST: can get you get a seat out?  yes
# seatdf %>%
#   dplyr::filter(last8 == 'R', 
#          last4 == 'R',
#          last2 == 'R') %>%
#   dplyr::select(yourseat)
# 
# rowdf %>%
#   dplyr::filter(last128 == 'B',
#                 last64 == 'B',
#                 last32 == 'B',
#                 last16 == 'B',
#                 last8 == 'B', 
#                 last4 == 'B',
#                 last2 == 'B') %>%
#   dplyr::select(yourrow)


#automate the FBLR
find_row <- function(rowstring){
  rowdf %>%
    dplyr::filter(last128 == rowstring[1],
                  last64 == rowstring[2],
                  last32 == rowstring[3],
                  last16 == rowstring[4],
                  last8 == rowstring[5], 
                  last4 == rowstring[6],
                  last2 == rowstring[7]) %>%
    dplyr::select(yourrow)
}

find_seat <- function(seatstring){
  seatdf %>% 
    dplyr::filter(last8 == seatstring[1], 
                  last4 == seatstring[2],
                  last2 == seatstring[3]) %>%
    dplyr::select(yourseat)
  
}

#put them together with the formula to calc the seatID
findID <- function(rowstring, seatstring){
  find_row(rowstring)*8 + find_seat(seatstring)
  
}


#test sample data
testrow <- c('B', 'F', 'F', 'F', 'B', 'B', 'F')
testseat <- c('R', 'R', "R")
find_row(testrow)*8 + find_seat(testseat)
findID(testrow, testseat)

#works for test case
#row number is 70
#seat number is 7
#total is 567  

###############################
#now: get data into strings so we can work with it


library(tidyverse)
dat <- read_delim("2020/Day05Data.txt", col_names = 'assignments', delim = '\n')

#split into rows and seat for the two functions
datb <- dat %>%
  mutate(rowloc = substring(assignments, 1, 7),
         seatloc = substring(assignments, 8, 10))

#create a list of character vectors for the rows and also for the seats
allrows <- strsplit(datb$rowloc, split = '?')
allseats <- strsplit(datb$seatloc, split= '?')

# #want to do this a billion times
# findID(allrows[[1]], allseats[[1]])

#use map2 to feed these two lists of vectors into the findID function
#list1= function arg 1, list 2 = functionarg2
#unlist lets you find the max value (otherwise what's the max list entry? meaningless)
map2(allrows, allseats, findID) %>% unlist %>% max

#myanswer: 906

#########################################################################################
##############################################################################################
# 
# --- Part Two ---
#   Ding! The "fasten seat belt" signs have turned on. Time to find your seat.
# 
# It's a completely full flight, so your seat should be the only missing boarding pass in your list. 
# However, there's a catch: some of the seats at the very front and back of the plane don't exist on this aircraft, 
# so they'll be missing from your list as well.
# 
# Your seat wasn't at the very front or back, though; 
# the seats with IDs +1 and -1 from yours will be in your list.
# 
# What is the ID of your seat?


#myseatID == myrow * 8 + myseat
#get a list of row and seats instead of the IDs?
#use table to look for low N (yeah, this isn't automated...)

map(allrows, find_row) %>% unlist %>% as_tibble %>% count(value) %>% filter(n < 8)
map(allseats, find_seat) %>% unlist %>% as_tibble %>% count(value) %>% filter(n < 128)
#some seats are missing in all locations, so let's not start there

#missing less than full rows, 6, 64, 113
#get seatIDs for all seats in row 64  (repeat for 6 and 113 if necessary)
data.frame(myrow = 64, myseat = rep(0:7)) %>%
  mutate(myID = myrow*8+myseat) %>% select(myID)


#method 1: look at list of seatIDs in the neighborhood and find a gap
#IDs for row 64 are from 512 to 519, so need to find seatsIDs 511:520 in the ID list
map2(allrows, allseats, findID) %>% unlist %>% sort
#we have 518 and 520, so my seat is 519

#method 2: I know what my row is (64) so find all related strings in the data
dat %>% filter(grepl(assignments, pattern = '^BFFFFFF')) %>% arrange(assignments)
#missing one is'BFFFFFFRRR'  #RRR = seat 7
#this one is simpler (less to look at), but you do have to figure out the seatcode
64*8+7 == 519

#i'm sure there is a better math way to do this