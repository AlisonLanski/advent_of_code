######
## Day 13, 2021

################

library(tidyverse)

day <- 13
run_type <- 'test'
run_type <- 'real'

if(run_type == 'test'){
  filepath <- here::here(paste0("2021/2021Day", day, "test.txt"))
  fp2 <- here::here(paste0("2021/2021Day", day, "test_folds.txt"))
} else {
  filepath <- here::here(paste0("2021/2021Day", day, ".txt"))
  fp2 <- here::here(paste0("2021/2021Day", day, "folds.txt"))
}

df <- read_csv(filepath, col_names = c("X", "Y"))

dffold <- read_delim(fp2, delim = "=", col_names = c("coord", "value")) %>%
  mutate(coord = str_remove_all(toupper(coord), pattern = 'FOLD ALONG '), 
         value = as.numeric(value))

folding <- function(df, axis, value){

  df <- df %>%
    mutate(counter = case_when(.data[[axis]] == value ~ 0,
                                     TRUE ~ .data$counter)) %>%
    mutate({{axis}} := case_when(.data[[axis]] < value ~ .data[[axis]],
                                      TRUE ~ .data[[axis]] - 2*(.data[[axis]]-value))) %>%
    distinct()
  return(df)
}

dat <- df %>% mutate(counter = 1)

#one fold: Part 1
onefold <- folding(dat, dffold$coord[1], dffold$value[1])
onefold %>% count(counter)

#all folds, then what letter shows? Part 2
#first, fold
for(i in 1:nrow(dffold)){
  dat <- folding(dat, dffold$coord[i], dffold$value[i])
}

#work on letters
dat %>% arrange(X, Y) %>%  View()

datmat <- dat %>%
  #r matrices are indexed from 1,1 to ... instead of from 0,0
  mutate(X = X+1,
         Y = Y+1)

#set up blank matrix
xmax <- max(datmat$X)
ymax <- max(datmat$Y)
matmat <- matrix('', nrow = xmax, ncol = ymax)

#replace blanks with #
for(i in 1:nrow(datmat)){
  matmat[datmat$X[i], datmat$Y[i]] <- '#'
}

#look at it -- turn on side!
matmat %>% t %>% View()
#EBLUBRFH
