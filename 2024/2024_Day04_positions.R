#########  Day 4
#################

################
##data 
library(tidyverse)
raw <- read_csv("2024/Data/Day04.txt", col_names = FALSE)

testing <- data.frame(X1 = c("MMMSXXMASM",
                             "MSAMXMSMSA",
                             "AMXSXMAAMM",
                             "MSAMASMSMX",
                             "XMASAMXAMM",
                             "XXAMMXXAMA",
                             "SMSMSASXSS",
                             "SAXAMASAAA",
                             "MAMMMXMMMM",
                             "MXMXAXMASX"))

################## 
# Instructions Part 1 
# This word search allows words to be
# horizontal, vertical, diagonal, written backwards, or even overlapping other
# words. It's a little unusual, though, as you don't merely need to find one
# instance of XMAS - you need to find all of them. do nothing. 
##################

############# 
# Need to set up 4 versions of the dataframe: 
# normal (left-right), up-down, diag \\\ and diag ///
# if we rotate to do this, we can use the same computation function each time


rotate_df <- function(df){
  # needs datchar type inputs
  rot_90 <- df %>% t() %>% data.frame() %>%
    unite(col = "X1", sep = "")
  
  return(rot_90)
}


create_diags <- function(df0, df90){
  ### needs datchar type inputs
  on45s <- bind_cols(df0[, 1], 
            map2(.x = df0[, c(2:ncol(df0))],
                 .y = seq_along(df0[, c(2:ncol(df0))]),
                 .f = ~lead(x = .x, n = .y))) %>% 
    replace(is.na(.), "Q") %>%
    unite(col = "X1", sep = "") %>%
    
    rbind(
      bind_cols(df90[, 1],
                map2(.x = df90[, c(2:ncol(df90))],
                     .y = seq_along(df90[, c(2:ncol(df90))]),
                     .f = ~lead(x = .x, n = .y))) %>%
        replace(is.na(.), "Q") %>%
        unite(col = "X1", sep = "")
    ) %>%
    tail(nrow(.)-1)
  
  return(on45s)
}


make_dfs <- function(df, which = "all"){
  ######
  # original
  normal <- df  #### RETURN THIS

  ######
  # need characters to rearrange in their own columns
  
  # regular order characters
  datchar <- str_split(df$X1, "", simplify = T) %>% data.frame()
  # rotate 90 (up-down)
  rot_90 <- rotate_df(datchar)  ## RETURN THIS
  # get 90 rotated chars (needed for diag)
  datchar_90 <- str_split(rot_90$X1, "", simplify = T) %>% data.frame()
  
  # swap character sides (needed for other diag)
  datchar_swapped <- rev(datchar)
  ### rotate again
  rot_180 <- rotate_df(datchar_swapped)
  #get more characters
  datchar_180 <- str_split(rot_180$X1, "", simplify = T) %>% data.frame()
 
  
  ### make diags  -- return both
  rot_45 <- create_diags(datchar, datchar_90)
  rot_135 <- create_diags(datchar_swapped, datchar_180)
  
  if(which == "all"){
    return(list = c(normal, rot_90, rot_45, rot_135))    
  } else if(which == "diags"){
    return(list = c(rot_45, rot_135))  
  } else
    return("ask for 'all' or 'diags'")

}



# first, read across forwards, then read across backwards and count.

counting <- function(dfcol){
  count_ltr <- map(dfcol, str_count, pattern = "XMAS") %>%
    reduce(sum)
  
  count_rtl <- map(dfcol, str_count, pattern = "SAMX") %>%
    reduce(sum)
  
  return(count_ltr + count_rtl)
}

# with test data, should give 18
make_dfs(testing) %>%
  map(., counting) %>%
  reduce(sum)

# with real data
make_dfs(raw) %>%
  map(., counting) %>%
  reduce(sum)

## 2560 -- correct!

################## 
# Instructions Part 2 
# You're supposed to find two MAS in the shape of an X. 
# Like this:  
#
# M S
#  A
# M S
#
# the diagonal MAS can go in any order
##################

#grid_search?

# make an even smaller DF for testing
df_extra <- data.frame(X1 = c("MMMS", 
                              "AAAA",
                              "SSSS",
                              "AAAA",
                              "MMMM"))
##### weirdness
## for whateve reason, df_extra and the testing df
## end up as character vectors in the midst of get_lists, 
## which means we can't have that mutate line, we just need to run substring directly
## but for the real data, we do have a dataframe
## so the code below now only works for RAW, but one change to that function and 
## it'll be fine for the testing data



# find the coordinates of As

findA <- function(df_extra){
  locate_A <- data.frame()
  for(i in 2:(nrow(df_extra)-1)){
    print(i)
    
    #find the end so we can remove it
    lastcol <- nchar(df_extra[1,])
    
    #find all the As, exclude the limits
    if(str_detect(df_extra[i,], pattern = "A")){
      locate_A <- str_locate_all(df_extra[i, ], pattern = "A") %>% 
        data.frame() %>% 
        transmute(rowpos = i, 
                  colpos = start) %>%
        bind_rows(locate_A) %>%
        filter(colpos > 1, colpos < lastcol) %>%
        filter(rowpos > 1)
        
    }
  }
  
  return(locate_A)
}



# cool, finding centrally located As.
As <- findA(raw)

# and now, get a list of little dfs 
get_lists <- function(As, df_extra){
  Alist <- list()
  
  for(k in 1:nrow(As)){
    i <- As$rowpos[k]
    j <- As$colpos[k]
    #print(i)
    #print(j)
    little_df <- df_extra[c((i-1):(i+1)),] %>% 
      mutate(newbit = substring(X1, first = j-1, last = j+1)) %>%
      transmute(X1 = newbit) %>%
      data.frame()
    #print(little_df)
    
    split_df <- str_split(little_df$X1, "", simplify = T) %>% data.frame()
    
    Alist <- c(Alist, list(split_df))
  }
  return(Alist)
}

Alist <- get_lists(As, raw)


#and ... check the diagonals?
#can do it by index position

ok <- logical()

testAs <- function(little_split){
  oneway <- c(little_split[1,1], little_split[2,2], little_split[3,3]) %>% paste0(collapse = "")
  otherway <- c(little_split[1,3], little_split[2,2], little_split[3,1])%>% paste0(collapse = "")
  #print(little_split)
  #print(oneway)
  #print(otherway)
  
  # true if X, false if not
  finding <- str_detect(oneway, pattern = "(MAS)|(SAM)") & str_detect(otherway, pattern = "(MAS)|(SAM)")
  ok <- c(ok, finding)
  #print(ok)
  return(ok)
}

# add em all up
map(Alist, testAs) %>%
  reduce(sum)

## 1910 -- correct!


