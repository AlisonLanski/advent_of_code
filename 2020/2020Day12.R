# #day 12
# 
# --- Day 12: Rain Risk ---
#   Your ferry made decent progress toward the island, but the storm came in faster than anyone expected. 
# The ferry needs to take evasive actions!
#   
#   Unfortunately, the ship's navigation computer seems to be malfunctioning; 
#   rather than giving a route directly to safety, it produced extremely circuitous instructions. 
#   When the captain uses the PA system to ask if anyone can help, you quickly volunteer.
# 
# The navigation instructions (your puzzle input) consists of 
# a sequence of single-character actions 
# paired with integer input values. 
# After staring at them for a few minutes, you work out what they probably mean:
# 
# Action N means to move north by the given value.
# Action S means to move south by the given value.
# Action E means to move east by the given value.
# Action W means to move west by the given value.
# Action L means to turn left the given number of degrees.
# Action R means to turn right the given number of degrees.
# Action F means to move forward by the given value in the direction the ship is currently facing.
# The ship starts by facing east. 
# Only the L and R actions change the direction the ship is facing. 
# (That is, if the ship is facing east and the next instruction is N10, the ship would move north 10 units, 
# but would still move east if the following action were F.)
# 
# For example:
# 
# F10
# N3
# F7
# R90
# F11
# 
# These instructions would be handled as follows:
# 
# F10 would move the ship 10 units east (because the ship starts by facing east) to east 10, north 0.
# N3 would move the ship 3 units north to east 10, north 3.
# F7 would move the ship another 7 units east (because the ship is still facing east) to east 17, north 3.
# R90 would cause the ship to turn right by 90 degrees and face south; it remains at east 17, north 3.
# F11 would move the ship 11 units south to east 17, south 8.
# At the end of these instructions, the ship's Manhattan distance 
# (sum of the absolute values of its east/west position and its north/south position) 
# from its starting position is 17 + 8 = 25.
# 
# Figure out where the navigation instructions lead. 
# What is the Manhattan distance between that location and the ship's starting position?


dat <- data.frame(instructions = c('F10', 'N3', 'F7', 'R90', 'F11'))

dat <- read_delim("2020/Day12Data.txt", col_names = "instructions", delim = "\t")


df <- dat %>% 
  mutate(direction = str_extract(instructions, pattern = "[A-Z]"),
              distance = as.numeric(str_remove_all(instructions, pattern = "[A-Z]")))

#make north and east positive, south and west negative
#make rotating left negative
df <- df %>% 
  mutate(distance = ifelse(direction %in% c("S", "W", "L"), distance*-1, distance))



#starting values
position_ns <- 0
position_ew <- 0
facing <- 90


#position functions
add_ew <- function(i){
  position_ew + df$distance[i]
}

add_ns <- function(i){
  position_ns + df$distance[i]
}


#flip sign:  if letter is F but facing is W or S, want negative
flip_sign <- function(i){
  df$distance[i]*-1
}

#facing function
rotate <- function(i){
  (facing+df$distance[i]) %% 360
}


#run through the df and execute one at a time


for(i in 1:nrow(df)){

    #if facing, add stuff
  if(df$direction[i] == "F"){
    if(facing == 90){
      position_ew <-  position_ew + df$distance[i]
    } 
    if(facing == 0) {
      position_ns <- position_ns + df$distance[i]
      }
    if(facing == 270) {
      position_ew <- position_ew - df$distance[i]
    }
    if(facing == 180) {
      position_ns <- position_ns - df$distance[i]
    }
  }
  
  #if rotating, rotate facing
  if(df$direction[i] %in% c("R", "L")) {
    facing <- rotate(i)
  }
  
  #if a cardinal direction facing, move position
  if(df$direction[i] %in% c("N", "S")){
    position_ns <- position_ns + df$distance[i]
  }
  
  if(df$direction[i] %in% c("E", "W")){
    position_ew <- position_ew + df$distance[i]
  }
}

position_ew
position_ns
facing

abs(position_ew) + abs(position_ns)
#works


###################
### Part 2
# 
# --- Part Two ---
#   Before you can give the destination to the captain, 
# you realize that the actual action meanings were printed on the back of the instructions the whole time.
# 
# Almost all of the actions indicate how to move a waypoint which is relative to the ship's position:
# 
# Action N means to move the waypoint north by the given value.
# Action S means to move the waypoint south by the given value.
# Action E means to move the waypoint east by the given value.
# Action W means to move the waypoint west by the given value.
# Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
# Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
# Action F means to move forward to the waypoint a number of times equal to the given value.
# The waypoint starts 10 units east and 1 unit north relative to the ship. 
# The waypoint is relative to the ship; that is, if the ship moves, the waypoint moves with it.
# 
# For example, using the same instructions as above:
# 
# F10 moves the ship to the waypoint 10 times (a total of 100 units east and 10 units north), 
# leaving the ship at east 100, north 10. 
# The waypoint stays 10 units east and 1 unit north of the ship.
# N3 moves the waypoint 3 units north to 10 units east and 4 units north of the ship. 
# The ship remains at east 100, north 10.
# F7 moves the ship to the waypoint 7 times (a total of 70 units east and 28 units north), 
# leaving the ship at east 170, north 38. 
# The waypoint stays 10 units east and 4 units north of the ship.
# R90 rotates the waypoint around the ship clockwise 90 degrees, 
# moving it to 4 units east and 10 units south of the ship. 
# The ship remains at east 170, north 38.
# F11 moves the ship to the waypoint 11 times 
# (a total of 44 units east and 110 units south), 
# leaving the ship at east 214, south 72. 
# The waypoint stays 4 units east and 10 units south of the ship.
# After these operations, the ship's Manhattan distance from its starting position is 214 + 72 = 286.
# 
# Figure out where the navigation instructions actually lead. 
# What is the Manhattan distance between that location and the ship's starting position?


#ship starts over
position_ns <- 0
position_ew <- 0
facing <- 90

  
#new starting position for the waypoint
waypoint_ns <- 1
waypoint_ew <- 10


# for(i in 1:nrow(df)){
i <- 6
  #if facing, move in waypoint multiples
  if(df$direction[i] == "F"){
      position_ew <-  position_ew + df$distance[i]*waypoint_ew
      position_ns <- position_ns + df$distance[i]*waypoint_ns
  }
  
  #if rotating, rotate facing and waypoint
  if(df$direction[i] %in% c("R", "L")) {

    facing <- (facing+df$distance[i]) %% 360
    
    if(abs(df$distance[i]) == 180){
      waypoint_ns <- -1*waypoint_ns
      waypoint_ew <- -1*waypoint_ew
      
    } else{
      if(facing == 180){
        temp_ns  <- waypoint_ew
        waypoint_ew <- abs(waypoint_ns)
        waypoint_ns <- -abs(temp_ns)
      }
      if(facing == 0){
        temp_ns  <- waypoint_ew
        waypoint_ew <- -abs(waypoint_ns)
        waypoint_ns <- abs(temp_ns)
      }
      if(facing == 270){
        temp_ns  <- waypoint_ew
        waypoint_ew <- -abs(waypoint_ns)
        waypoint_ns <- -abs(temp_ns)
        
      }
      if(facing == 90){
        temp_ns  <- waypoint_ew
        waypoint_ew <- abs(waypoint_ns)
        waypoint_ns <- abs(temp_ns)
        
      }
    }
  }
  
  #if a cardinal direction, move waypoint
  if(df$direction[i] %in% c("N", "S")){
    waypoint_ns <- waypoint_ns + df$distance[i]
  }
  
  if(df$direction[i] %in% c("E", "W")){
    waypoint_ew <- waypoint_ew + df$distance[i]
  }
# }

position_ew
position_ns
facing
waypoint_ew
waypoint_ns
abs(position_ew) + abs(position_ns)
#38003 too low
