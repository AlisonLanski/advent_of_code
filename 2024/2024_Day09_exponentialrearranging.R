#########  Day 9
#################

################
##data 
library(tidyverse)
raw <- read_file("2024/Data/Day09.txt") 

#easier as letters?
testing <- "2333133121414131402" 
 
df <- testing
df <- raw

################## 
# Instructions Part 1 
#
# He shows you the disk map (your puzzle input) he's already generated. For example:
# 
# 2333133121414131402
# The disk map uses a dense format to represent the layout of files and 
# free space on the disk. The digits alternate between indicating the length of 
# a file and the length of free space.
# 
# So, a disk map like 12345 would represent a one-block file, 
#  two blocks of free space, a three-block file, 
#  four blocks of free space, and then a five-block file. 
#  A disk map like 90909 would represent three nine-block files in a row 
#  (with no free space between them).
# 
# Each file on disk also has an ID number based on the order of the files 
#  as they appear before they are rearranged, starting with ID 0. 
#  So, the disk map 12345 has three files: a one-block file with ID 0, 
#  a three-block file with ID 1, and a five-block file with ID 2. 
#  Using one character for each block where digits are the file ID and . is 
#  free space, the disk map 12345 represents these individual blocks:
#   
#   0..111....22222
# The first example above, 2333133121414131402, represents these individual blocks:
#   
#   00...111...2...333.44.5555.6666.777.888899
# The amphipod would like to move file blocks one at a time from the end of the 
#  disk to the leftmost free space block (until there are no gaps remaining 
#  between file blocks). For the disk map 12345, the process looks like this:
#   
# 0..111....22222
# 02.111....2222.
# 022111....222..
# 0221112...22...
# 02211122..2....
# 022111222......
# The first example requires a few more steps:
#   
# 00...111...2...333.44.5555.6666.777.888899
# 009..111...2...333.44.5555.6666.777.88889.
# 0099.111...2...333.44.5555.6666.777.8888..
# 00998111...2...333.44.5555.6666.777.888...
# 009981118..2...333.44.5555.6666.777.88....
# 0099811188.2...333.44.5555.6666.777.8.....
# 009981118882...333.44.5555.6666.777.......
# 0099811188827..333.44.5555.6666.77........
# 00998111888277.333.44.5555.6666.7.........
# 009981118882777333.44.5555.6666...........
# 009981118882777333644.5555.666............
# 00998111888277733364465555.66.............
# 0099811188827773336446555566..............
#
# The final step of this file-compacting process is to update the filesystem 
#  checksum. To calculate the checksum, add up the result of multiplying 
#  each of these blocks' position with the file ID number it contains. 
#  The leftmost block is in position 0. If a block contains free space, 
#  skip it instead.
# 
# Continuing the first example, the first few blocks' position multiplied by its
#  file ID number are 0 * 0 = 0, 1 * 0 = 0, 2 * 9 = 18, 3 * 9 = 27, 4 * 8 = 32, 
#  and so on. In this example, the checksum is the sum of these, 1928.
# 
# Compact the amphipod's hard drive using the process he requested. 
#  What is the resulting filesystem checksum?
#
##################

#### So we need to keep track of the math as we go, I think.
#orig <- df
df <- orig

df
df_back <- df

nchar(df)
nchar(df_back)

total <- 0

final_position <- 0
orig_position <- 0

#### Loop here
print(df)
print(df_back)
if (orig_position %% 2 == 0){
  blocktype = "values"
} else {
  blocktype = "spaces"
}


front <- str_extract(df, pattern = "^[0-9]")
how_many_front = as.numeric(front)
back <- str_extract(df_back, pattern = "[0-9]$")
how_many_back = as.numeric(back)
back_position = floor(nchar(df_back)/2)


if(blocktype == "values"){
  for (i in 0:(how_many_front-1)){
    total = total + (final_position+i)*orig_position
  }
  df <- str_remove(df, pattern = "^[0-9]")
  final_position <- final_position + how_many_front
  orig_position <- orig_position + 1
} else {
  if(how_many_front == how_many_back){
    for (i in 0:(how_many_front-1)){
      total = total + (final_position+i)*back_position
    }
    df <- str_remove(df, pattern = "^[0-9]")
    final_position <- final_position + how_many_front
    orig_position <- orig_position + 1
    df_back <- str_remove(df_back, pattern = "[0-9][0-9]$") 
    df <- str_remove(df, pattern = "[0-9][0-9]$")
  } else if(how_many_front < how_many_back){
      for (i in 0:(how_many_front-1)){
        total = total + (final_position+i)*back_position
      }
      df <- str_remove(df, pattern = "^[0-9]")
      final_position <- final_position + how_many_front
      orig_position <- orig_position + 1
      df_back <- str_replace(df_back, 
                             pattern = "[0-9]$", 
                             replacement = as.character(how_many_back-how_many_front)) 
  } else {
    for (i in 0:(how_many_back-1)){
      total = total + (final_position+i)*back_position
    }
    df <- str_replace(df, 
                      pattern = "^[0-9]", 
                      replacement = as.character(how_many_front-how_many_back))
    final_position <- final_position + how_many_front
    #orig_position <- orig_position
    df_back <- str_remove(df_back, pattern = "[0-9][0-9]$")
    df <- str_remove(df, pattern = "[0-9][0-9]$")
  }
}

print(total)

#final after looping is way too high? Or I'm checking the wrong input
# maybe check the short one?
################## 
# Instructions Part 2 
#
# 
##################

