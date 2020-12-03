# #Day 3
# 
# --- Day 3: Toboggan Trajectory ---
#   With the toboggan login problems resolved, you set off toward the airport. While travel by toboggan might be easy, it's certainly not safe: there's very minimal steering and the area is covered in trees. You'll need to see which angles will take you near the fewest trees.
# 
# Due to the local geology, trees in this area only grow on exact integer coordinates in a grid. You make a map (your puzzle input) of the open squares (.) and trees (#) you can see. For example:
# 
# ..##.......
# #...#...#..
# .#....#..#.
# ..#.#...#.#
# .#...##..#.
# ..#.##.....
# .#.#.#....#
# .#........#
# #.##...#...
# #...##....#
# .#..#...#.#
# These aren't the only trees, though; due to something you read about once involving arboreal genetics and biome stability, the same pattern repeats to the right many times:
#   
#   ..##.........##.........##.........##.........##.........##.......  --->
# #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
# .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
# ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
# .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
# ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
# .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
# .#........#.#........#.#........#.#........#.#........#.#........#
# #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
# #...##....##...##....##...##....##...##....##...##....##...##....#
# .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
# You start on the open square (.) in the top-left corner and need to reach the bottom (below the bottom-most row on your map).
# 
# The toboggan can only follow a few specific slopes (you opted for a cheaper model that prefers rational numbers); start by counting all the trees you would encounter for the slope right 3, down 1:
#   
#   From your starting position at the top-left, check the position that is right 3 and down 1. Then, check the position that is right 3 and down 1 from there, and so on until you go past the bottom of the map.
# 
# The locations you'd check in the above example are marked here with O where there was an open square and X where there was a tree:
# 
# ..##.........##.........##.........##.........##.........##.......  --->
# #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
# .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
# ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
# .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
# ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
# .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
# .#........#.#........X.#........#.#........#.#........#.#........#
# #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
# #...##....##...##....##...#X....##...##....##...##....##...##....#
# .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
# In this example, traversing the map using this slope would cause you to encounter 7 trees.
# 
# Starting at the top-left corner of your map and following a slope of right 3 and down 1, 
# how many trees would you encounter?

library(tidyverse)

dat <- read_delim("2020/Day03Data.txt", delim = "\n", col_names = "All")
head(dat)

nrow(dat)

#this will find the position in each row
str_locate_all(dat$All[1], pattern = '#')[[1]][,1]

#make it binary
datb <- dat %>% 
  mutate(All = str_replace_all(All, pattern = "\\.", replacement = '0'),
         All = str_replace_all(All, pattern = "#", replacement = '1'))

#how far to go over for each row down
n <- 3

#implement test: n*rownumber for how far over, mod it by 31 to get the repeated aspect
for (i in 1:5){
  #starting position for each row
    p <- n*(i-1) +1 
    print(i)
    print(n)
    print(p)
  
  if(p < 31){
    print(substring(datb$All[i], p, p))  
  } 
  else if (p %% 31 == 0){
    print(substring(datb$All[i], 31, 31))
  } else {
    print(substring(datb$All[i], first = p %% 31, last = p %% 31))
  }
}

#BUT -- we have to start in position 1, not postion 0
#AND in row 1 we have to start at position 1, not position 4

#only keep the 1s and increment
n <- 3
counter <- 0
for (i in 1:nrow(datb)){
  p <- n*(i-1) +1
  
  if(p < 31 & substring(datb$All[i], p, p) == 1){
    counter <- counter + 1

  } else if (p %% 31 == 0 & substring(datb$All[i], 31, 31) == 1){
    counter <- counter + 1  #have to use the else bc if you just use if you can double-count
    
  } else {
    if (substring(datb$All[i], first = p %% 31, last = p %% 31) == 1){
      counter <- counter + 1
    }
  }
}
counter



##########################################
# 
# --- Part Two ---
#   Time to check the rest of the slopes - 
#  you need to minimize the probability of a sudden arboreal stop, after all.
# 
# Determine the number of trees you would encounter if, 
#  for each of the following slopes, you start at the top-left corner and traverse the map all the way to the bottom:
#   
# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.
# In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s) respectively; 
# multiplied together, these produce the answer 336.
# 
# What do you get if you multiply together the number of trees encountered on each of the 
# listed slopes?

#so we're doing the same thing but with additional ns and then additional p
#write a function

#have to account for multi-downs
#need a separate counter for row and col
#the weird calc that does row/position at the same time doesn't play nicely
treeslope <- function(over, down){
  counter <- 0
  r <- 1
  p <- 1
  nmod <- nchar(datb$All[1])
  
    while(r < nrow(datb)){
      r <- r + down
      p <- p + over
      p <- p %% nmod
      
      if (p == 0 & substring(datb$All[r], nmod, nmod) == 1){
      counter <- counter + 1  
      
      } else {
        if(substring(datb$All[r], first = p, last = p) == 1){
            counter <- counter + 1
        }
      }
      # r <- r + down  #if you put it here instead, it doesn't work; still figuring out why
      # p <- p + over
    }
  return(counter)
}

t1 <- treeslope(over = 1, down = 1) #88
t2 <- treeslope(over = 3, down = 1) #145
t3 <- treeslope(over = 5, down = 1) #71
t4 <- treeslope(over = 7, down = 1) #90
t5 <- treeslope(over = 1, down = 2) #42

t1
t2
t3
t4
t5

t1*t2*t3*t4*t5


# #for testing
# datc <- read_delim("2020/Day03DataTest.txt", delim = '\n', col_names = "All")
# datb <- datc %>% 
#   mutate(All = str_replace_all(All, pattern = "\\.", replacement = '0'),
#          All = str_replace_all(All, pattern = "#", replacement = '1'))
