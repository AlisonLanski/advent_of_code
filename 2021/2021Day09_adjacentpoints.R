########
## Advent of code, 2021, Day 09

######
## Setup

#load
library(tidyverse)

runtype <- rstudioapi::showPrompt(title = "Type of run", message = "'test' or 'real'?")
day <- "09"

if(runtype == 'test'){
  filename <- paste0("2021Day", day, "test.txt")
} else {
  filename <- paste0("2021Day", day, ".txt")
}

df <- read_table(file = here::here("2021", filename), col_names = FALSE, col_types = "c")#, trim_ws = TRUE)
dat <-  str_split_fixed(df$X1, n = nchar(df$X1[1]), pattern = '')

rm(filename, day, runtype)


#########
### Find the low spots
## not elegant, but it works
## better to do something more generic that can crawl around and handle edge spots
rr <- nrow(dat)
cc <- ncol(dat)
lowheights <- numeric()

#left
for(i in 2:(rr-1)){
  if(
    ((dat[i,1] < dat[i,2]) & 
     (dat[i,1] < dat[i-1, 1]) & 
     (dat[i,1] < dat[i+1, 1])) == TRUE){
    print(paste0(dat[i,1], " at (", i, ",", 1, ")"))
    lowheights <- c(lowheights, dat[i,1])
  }    
}

#top
for(j in 2:(cc-1)){
  if(
    ((dat[1,j] < dat[1,j+1]) & 
     (dat[1,j] < dat[1,j-1]) & 
     (dat[1,j] < dat[2,j])) == TRUE){
    print(paste0(dat[1,j], " at (", 1, ",", j, ")"))
    lowheights <- c(lowheights, dat[1,j])
  }    
}

#bottom
for(j in 2:(cc-1)){
  if(
    ((dat[rr,j] < dat[rr,j+1]) & 
     (dat[rr,j] < dat[rr,j-1]) & 
     (dat[rr,j] < dat[rr-1,j])) == TRUE){
    print(paste0(dat[rr,j], " at (", rr, ",", j, ")"))
    lowheights <- c(lowheights, dat[rr,j])
  }    
}

#right
for(i in 2:(rr-1)){
  if(
    ((dat[i,rr] < dat[i,rr-1]) & 
     (dat[i,rr] < dat[i-1, rr]) & 
     (dat[i,rr] < dat[i+1, rr])) == TRUE){
    print(paste0(dat[i,rr], " at (", i, ",", rr, ")"))
    lowheights <- c(lowheights, dat[i,rr])
  }    
}

#topleft corner
if(((dat[1,1] < dat[1,2]) & 
   (dat[1,1] < dat[2,1])) == TRUE){
  print(paste0(dat[1,1], " at (", 1, ",", 1, ")"))
  lowheights <- c(lowheights, dat[1,1])
}

#topright corner
if(((dat[1,cc] < dat[1,cc-1]) & 
    (dat[1,cc] < dat[2,cc])) == TRUE){
  print(paste0(dat[1,cc], " at (", 1, ",", cc, ")"))
  lowheights <- c(lowheights, dat[1,cc])
}

#bottomleft corner
if(((dat[rr,1] < dat[rr,2]) & 
    (dat[rr,1] < dat[rr-1,1])) == TRUE){
  print(paste0(dat[rr,1], " at (", rr, ",", 1, ")"))
  lowheights <- c(lowheights, dat[rr,1])
}

#bottomright corner
if(((dat[rr,cc] < dat[rr,cc-1]) & 
    (dat[rr,cc] < dat[rr-1,cc])) == TRUE){
  print(paste0(dat[rr,cc], " at (", rr, ",", cc, ")"))
  lowheights <- c(lowheights, dat[rr,cc])
}

#inside
for(i in 2:(rr-1)){
  for(j in 2:(cc-1)){
    if(
      ((dat[i,j] < dat[i,j-1]) & 
      (dat[i,j] < dat[i, j+1]) & 
      (dat[i,j] < dat[i-1, j]) & 
      (dat[i,j] < dat[i+1, j])) == TRUE){
      print(paste0(dat[i,j], " at (", i, ",", j, ")"))
      lowheights <- c(lowheights, dat[i,j])
    }    
  }
}

sum(as.numeric(lowheights)+1)

####
## Find size of areas bounded by 9 (or edges)
## Multiply the 3 largest areas together (size does not include the 9s)

newr <- nrow(dat)+1
newc <- ncol(dat)+1
#newr <- 6
#newc <- 101

ninespots <- which(dat == 9, arr.ind = TRUE)
otherspots <- which(dat != 9, arr.ind = TRUE)
lettercodes <- c(letters, 
                 paste0('a', letters), 
                 paste0('b',letters), 
                 paste0('c',letters),
                 paste0('d',letters),
                 paste0('e',letters),
                 paste0('f',letters),
                 paste0('g',letters),
                 paste0('h',letters),
                 paste0('i',letters),
                 paste0('j',letters),
                 paste0('k',letters),
                 paste0('l',letters),
                 paste0('m',letters),
                 paste0('n',letters),
                 paste0('o',letters),
                 paste0('p',letters),
                 paste0('q',letters),
                 paste0('r',letters),
                 paste0('s',letters),
                 paste0('t',letters),
                 paste0('u',letters))
dat2 <- dat
dat2[otherspots] <- 0 
dat2 <- data.frame(dat2)
bottoms <- data.frame(t(matrix(rep('9', newc-1))))
colnames(bottoms) <- colnames(dat2)
dat2 <- dat2 %>% bind_rows(bottoms, ., bottoms) %>%  cbind(Frst = 9, ., Xtra = 9)

#set letters down and to the right
areacount <- 0

for(i in 2:newr) {
  for(j in 2:newc){
    print(paste0(i, ",", j))
    if(dat2[i, j] == 0){
      if((dat2[i-1, j] %in% lettercodes)| (dat2[i, j-1] %in% lettercodes) | (dat2[i, j+1] %in% lettercodes) |
         dat2[i+1, j] %in% lettercodes){
        letter <- min(str_replace_all(c(dat2[i-1, j], dat2[i, j-1], dat2[i+1, j], dat2[i, j+1]), '9', 'ZZZ'))
      } else {
        areacount <- areacount+1
        letter <- lettercodes[areacount]
      }
      dat2[i,j] <- letter
      k <- i+1
      m <- j+1
      while(dat2[k,j] == 0){
        dat2[k,j] <- letter
        k <- k +1
      }
      while(dat2[i,m] == 0){
        dat2[i,m] <- letter
        m <- m+1
      }
    }
  }
}
dat2[2:6, 2:11]

dat2 %>% unlist %>% table() %>% sort()
#three largest = 9*9*14 for test
#three largest = 75*72*66 for real --- too low -- areas are filled but letters are touching 
