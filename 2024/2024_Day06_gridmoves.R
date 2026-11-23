#########  Day 6
#################

################
##data 
library(tidyverse)
raw <- read_delim("2024/Data/Day06.txt", delim = "\\n", col_names = "spots") %>%
  mutate(spots = str_replace_all(spots, pattern = "\\.", replacement = "O"),
         spots = str_replace_all(spots, pattern = "#", replacement = "B"),
         spots = str_replace_all(spots, pattern = "\\^", replacement = "X")) %>%
  separate(col = "spots", into = c(paste0("X", 0:130)), sep = "") %>%
  select(-X0) %>%
  as.matrix()

# get a nice text-based format for the input data
testing <- data.frame(spots = c("....#.....",
                                ".........#",
                                "..........",
                                "..#.......",
                                ".......#..",
                                "..........",
                                ".#..^.....",
                                "........#.",
                                "#.........",
                                "......#...")) %>% 
  mutate(spots = str_replace_all(spots, pattern = "\\.", replacement = "O"),
         spots = str_replace_all(spots, pattern = "#", replacement = "B"),
         spots = str_replace_all(spots, pattern = "\\^", replacement = "X")) %>%
  separate(col = "spots", into = c(paste0("X", 0:10)), sep = "") %>%
  select(-X0) %>%
  as.matrix()


df <- testing
df <- raw


################## 
# Instructions Part 1 
#
# Lab guards in 1518 follow a very strict patrol protocol which involves 
# repeatedly following these steps:
# - If there is something directly in front of you, turn right 90 degrees.
# - Otherwise, take a step forward.
# Predict the path of the guard. How many distinct positions will the guard 
# visit before leaving the mapped area?
# 
##################


### save these to use below
nr <- nrow(df)
nc <- ncol(df)

# set up initial values
location <- which(df=="X", arr.ind = T)
direction <- "up"

# function to find the "next spot"
find_next <- function(location, direction){
  loc_row <- location[1]
  loc_col <- location[2]
  
  if(direction == "up"){
    find_row <- loc_row - 1
    find_col <- loc_col
  }else if(direction == "down"){
    find_row <- loc_row + 1
    find_col <- loc_col
  }else if(direction == "right"){
    find_row <- loc_row
    find_col <- loc_col+1
  }else{
    find_row <- loc_row
    find_col <- loc_col-1
  }
  
  next_location <- location
  next_location[1] <- find_row
  next_location[2] <- find_col
  
  return(next_location)
}


# function to stay where you are (undo next) if it's blocked
go_back <- function(location, direction){
  loc_row <- location[1]
  loc_col <- location[2]
  
  if(direction == "up"){
    find_row <- loc_row + 1
    find_col <- loc_col
  }else if(direction == "down"){
    find_row <- loc_row - 1
    find_col <- loc_col
  }else if(direction == "right"){
    find_row <- loc_row
    find_col <- loc_col-1
  }else{
    find_row <- loc_row
    find_col <- loc_col+1
  }
  
  next_location <- location
  next_location[1] <- find_row
  next_location[2] <- find_col
  
  return(next_location)
}

## use all info to make an appropriate move
check_sit <- function(matrix, location, direction){
  find_row <- location[1]
  find_col <- location[2]
  # if we are still in bounds
  if((find_row > 0) & (find_row <= nr) & (find_col > 0) & find_col <= nc){
    # check if the next spot is "open", and move into it
    if(matrix[find_row, find_col] %in% c("O", "X")){
      loc_row <- find_row
      loc_col <- find_col
      matrix[loc_row, loc_col] <- "X"
      # or verify that the next spot is blocked and rotate appropriately
    }else if(matrix[find_row, find_col] == "B"){
      
      # stay in current location (don't use the new one)
      location <- go_back(location, direction)
      
      # change directions
      direction <- case_when(direction == "up" ~ "right",
                             direction == "right" ~ "down", 
                             direction == "down" ~ "left",
                             direction == "left" ~ "up")
    }else{
      print(paste("in bounds with problem, from position", 
                  loc_row, ",", loc_col,
                  "going", direction, "to position",
                  find_row, ",", find_col))
    }
    return(list(matrix, direction, location, FALSE))
    # if not in bounds, here we are!
  }else{
    print("escaped!")
    return(list(matrix, direction, location, TRUE))
  }
  
  #end of function
}


# use a loop to solve
doneyet <- FALSE
while(!doneyet){
  location <- find_next(location, direction)
  results <- check_sit(df, location, direction)
  df <- results[[1]]
  direction <- results[[2]]
  location <- results[[3]]
  doneyet <- results[[4]]
  print(location)
}

#get the answer
sum(str_count(df, "X"))




################## 
# Instructions Part 2 
#
# You need to get the guard stuck in a loop by adding a single new obstruction.
# How many different positions could you choose for this obstruction?
# 
##################


### ooof.  testing input value is 6,
### to do this, we might need to know the direction we're going at each X point?
### could change that easily by using a position label instead of X (NSEW)
### but still need to figure out how to ID that it's an appropriate location
### "is there a path to my right, in my right-turn direction?" would work
### but that doesn't seem sufficient to catch all possibilities
### like in the testing set, they also block the exit point
###   and they also have a row where... if turned right and went up a few, you
###   run into an apppropriate path.
###   hmmm so maybe you want to see if there are any path-appropriate items
###   in your "to the right" row or column, before you see a block
### and the block has to be placed NOT in your future spot, but 1 after it,
###  so you still go into your future spot, but then in that location you turn


### i think you don't need to track direction at each point, but you do need
### to know which points are traveled.  And then test what happens if you block
### each one of them, one at a time.  Do you loop? Or escape?
### Not sure of the right way to check for looping.  
### Maybe if the path stabilizes at a particular number of X with no escape. 

# i am certain there is a better way to do this, but here's where i am!