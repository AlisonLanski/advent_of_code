# #Day 10
# 
# --- Day 10: Adapter Array ---
#   Patched into the aircraft's data port, you discover weather forecasts of a massive tropical storm. 
#  Before you can figure out whether it will impact your vacation plans, however, your device suddenly turns off!
# 
# Its battery is dead.
# 
# You'll need to plug it in. There's only one problem: 
# the charging outlet near your seat produces the wrong number of jolts. 
# Always prepared, you make a list of all of the joltage adapters in your bag.
# 
# Each of your joltage adapters is rated for a specific output joltage (your puzzle input). 
# Any given adapter can take an input 1, 2, or 3 jolts lower than its rating and still produce its rated output joltage.
# 
# In addition, your device has a built-in joltage adapter rated for 3 jolts higher than the highest-rated adapter in your bag. 
# (If your adapter list were 3, 9, and 6, your device's built-in adapter would be rated for 12 jolts.)
# 
# Treat the charging outlet near your seat as having an effective joltage rating of 0.
# 
# Since you have some time to kill, you might as well test all of your adapters. 
# Wouldn't want to get to your resort and realize you can't even charge your device!
#   
#   If you use every adapter in your bag at once, what is the distribution of joltage 
#  differences between the charging outlet, the adapters, and your device?
#   
#   For example, suppose that in your bag, you have adapters with the following joltage ratings:
#   
# 16
# 10
# 15
# 5
# 1
# 11
# 7
# 19
# 6
# 12
# 4
# With these adapters, your device's built-in joltage adapter would be rated for 19 + 3 = 22 jolts, 
#  3 higher than the highest-rated adapter.
# 
# Because adapters can only connect to a source 1-3 jolts lower than its rating, 
# in order to use every adapter, you'd need to choose them like this:
#   
# The charging outlet has an effective rating of 0 jolts, 
# so the only adapters that could connect to it directly would need to have a joltage rating of 1, 2, or 3 jolts. 
# Of these, only one you have is an adapter rated 1 jolt (difference of 1).
# From your 1-jolt rated adapter, the only choice is your 4-jolt rated adapter (difference of 3).
# From the 4-jolt rated adapter, the adapters rated 5, 6, or 7 are valid choices. 
# However, in order to not skip any adapters, you have to pick the adapter rated 5 jolts (difference of 1).
# Similarly, the next choices would need to be the adapter rated 6 and then the adapter rated 7 (with difference of 1 and 1).
# The only adapter that works with the 7-jolt rated adapter is the one rated 10 jolts (difference of 3).
# From 10, the choices are 11 or 12; choose 11 (difference of 1) and then 12 (difference of 1).
# After 12, only valid adapter has a rating of 15 (difference of 3), then 16 (difference of 1), then 19 (difference of 3).
# Finally, your device's built-in adapter is always 3 higher than the highest adapter, 
#  so its rating is 22 jolts (always a difference of 3).
# In this example, when using every adapter, there are 7 differences of 1 jolt and 5 differences of 3 jolts.
# 
# Here is a larger example:
# 
# 28
# 33
# 18
# 42
# 31
# 14
# 46
# 20
# 48
# 47
# 24
# 23
# 49
# 45
# 19
# 38
# 39
# 11
# 1
# 32
# 25
# 35
# 8
# 17
# 7
# 9
# 4
# 2
# 34
# 10
# 3
# In this larger example, in a chain that uses all of the adapters, 
# there are 22 differences of 1 jolt and 10 differences of 3 jolts.
# 
# Find a chain that uses all of your adapters to connect the charging outlet to your device's 
# built-in adapter and count the joltage differences between the charging outlet, 
# the adapters, and your device. 
# What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?

testdat <- c(16,
  10,
  15,
  5,
  1,
  11,
  7,
  19,
  6,
  12,
  4)

testdat <- c(28,
  33,
  18,
  42,
  31,
  14,
  46,
  20,
  48,
  47,
  24,
  23,
  49,
  45,
  19,
  38,
  39,
  11,
  1,
  32,
  25,
  35,
  8,
  17,
  7,
  9,
  4,
  2,
  34,
  10,
  3
)

dat <- sort(testdat)


library(tidyverse)
#real data
dat <- sort(as.numeric(read_lines("2020/Day10Data.txt")))

#just need to take the differences between each adaptor in order and figure out the gap-size
#sort it and add the starter joltage
#remove the front and add the final on back
dats <- c(0, dat)
datmv <- c(dats[-1],max(dats)+3)
#subtract and table to get results
table(datmv - dats)

#multiply results:
65*34


#############################
# 
# #part 2
# 
# --- Part Two ---
#   To completely determine whether you have enough adapters, you'll need to figure out how many 
#   different ways they can be arranged. 
#   Every arrangement needs to connect the charging outlet to your device. 
#   The previous rules about when adapters can successfully connect still apply.
# 
# The first example above (the one that starts with 16, 10, 15) supports the following arrangements:
# 
# (0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
# (0), 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, (22)
# (0), 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, (22)
# (0), 1, 4, 5, 7, 10, 12, 15, 16, 19, (22)
# (0), 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, (22)
# (0), 1, 4, 6, 7, 10, 12, 15, 16, 19, (22)
# (0), 1, 4, 7, 10, 11, 12, 15, 16, 19, (22)
# (0), 1, 4, 7, 10, 12, 15, 16, 19, (22)
# (The charging outlet and your device's built-in adapter are shown in parentheses.) 
# Given the adapters from the first example, the total number of arrangements 
# that connect the charging outlet to your device is 8.
# 
# The second example above (the one that starts with 28, 33, 18) has many arrangements. 
# Here are a few:
#   
#   (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
# 32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 48, 49, (52)
# 
# (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
# 32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 49, (52)
# 
# (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
# 32, 33, 34, 35, 38, 39, 42, 45, 46, 48, 49, (52)
# 
# (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
# 32, 33, 34, 35, 38, 39, 42, 45, 46, 49, (52)
# 
# (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
# 32, 33, 34, 35, 38, 39, 42, 45, 47, 48, 49, (52)
# 
# (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
# 46, 48, 49, (52)
# 
# (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
# 46, 49, (52)
# 
# (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
# 47, 48, 49, (52)
# 
# (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
# 47, 49, (52)
# 
# (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
# 48, 49, (52)
# In total, this set of adapters can connect the charging outlet 
# to your device in 19208 distinct arrangements.
# 
# You glance back down at your bag and try to remember why you brought so many adapters; 
# there must be more than a trillion valid ways to arrange them! 
#   Surely, there must be an efficient way to count the arrangements.
# 
# What is the total number of distinct ways you can arrange the adapters 
# to connect the charging outlet to your device?



#we aren't rearranging, we are dropping out ones that can be skipped
#skipping can also happen in combination
#can't skip the 3s
#can't skip the edge-ones

#make the difference vector
datd <- datmv - dats

#find the 3s
threefixed <- which(datd == 3)

#these are the positions where we have a 1 followed by a 3
#they also can't be removed
onefixed <- threefixed -1

#so these are the unremoveable positions
#we get dups, because of the 3, 3; get rid of that
fixed <- unique(sort(c(onefixed, threefixed)))

#find the length of the gaps by looking at indices again
# (the gaps are where the removeable chains are)
fixeds <- c(0, fixed)
fixedmv  <- c(fixeds[-1], max(fixeds)+1)

#shows how many numbers are in the gaps and how often each set of numbers happens
#lots of 0s for consecutive numbers
# groups of 1  contribute 2 options (on/off)
# groups of 2 contribute 2 options each (on/off)
# groups of 3 contribute 2 options each (on/off) -1 because they can't be all-off
# (then the gap is too big)
# so  we count onesies each as 2, each twosies as 4, each threesies as 7
#subtracting ones makes sure we don't count the gap boundary, only the numbers in the gap
table(fixedmv - fixeds -1)
(1*2)^5 * (2*2)^2 * (2*2*2 -1)^12


# #don't need manual method anymore :}  
# #these are the removeable positions
# removing <- setdiff(which(datd > 0), fixed)
# 
# 
# #programmatic is hard -- just look at it?
# removing
# combos <- c(3, 1, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 1, 2, 3, 1, 1, 1, 3)
# table(combos)
# options(scipen = 99)
# (2^5) * (4^2) * (7^12) 


# #old stuff when i thought we were doing permutations, not combinations
# think about this: when the difference is 3 you don't have other options and the 1 has to come before the 3.
#when you have 13 you have to take that as-is
#when you have 33 you hvae to take that as-is
#when you have 111 you can go 
#   123 (all cases) or 
#   132 (all cases, if not followed by a 3) or 
#   321 (all cases, if not followed by a 3)


# when it's 1, you can go in order, or skip around, but there are limited options
# 1 1 1 1
# can go  1234, 1243,   

#want to break this into segments and then multiply the possible combinations of those segements together

# #if we have an isolated 1, that is also fixed
# #eg 3 1 1 3
# #find those by... doing that shifty thing
# #remove the front and add the final on back
# datmv1 <- c(removing[-1],max(removing)+1)
# #subtract and find indices of onesies
# onesies <- removing[which(datmv1 - removing > 1)+1]
# 
# #find twosies
# removingv2 <- setdiff(removing, onesies)
# 
# #but this shows us 1-way shift -- need to do a 2-way shift
# #how do we do that? shift each way and compare
# datmvr <- c(0, moving_prelim)[1:which.max(moving_prelim)]
# extrafixed <- moving_prelim[(datmv - moving_prelim > 1) & (moving_prelim - datmvr > 1)]
# 
# #update fixed list
# fixed <- c(fixed_prelim, extrafixed) %>% sort() %>% unique()
# 
# #update moving list
# moving <- setdiff(moving_prelim, extrafixed)
# 
# #how big are the runs?
# #find the run boundaries
# breakpoints <- integer()
# for(i in 1:(length(removing)-1)){
#   if(removing[i]+1 != removing[i+1]){
#     #find the indices of the last number in each run
#     breakpoints <- c(breakpoints, i)
#   }
# }
# 
# #do that shifty thing again to get the length of runs
# breakpointsmv <- c(breakpoints[-1], 100)
# table(breakpointsmv-breakpoints)
# 
# #7 strings of 2
# #6 strings of 3
# #2 strings of 4
# 
# #string of 2 and 3 can happen in any order
# #string of 4... factorial still okay because none of the gaps are >3
# #then you can have any one of the first followed by any one of the second followed by any one of the third
# (factorial(2) *7) * (factorial(3) *6) * (factorial(4) * 2)
# view(dat)
# view(sort(dat))
# 
# dat
