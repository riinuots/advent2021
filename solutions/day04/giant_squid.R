library(tidyverse)
rm(list= ls())
numbers = scan("solutions/day04/input_numbers", sep = ",")

# read the input into matrixes, I'm sure there must be an easier way to do this...
boards_raw = read.table("solutions/day04/input_boards")
n_boards = nrow(boards_raw)/5
boards = boards_raw %>% 
  pivot_longer(everything()) %>% 
  pull(value) %>% 
  array(dim = c(5, 5, n_boards))
# my columns are rows are the wrong way roung but...let's just go with it.
# I'll tilt my head :)

# Figure out how to calculate row and column sums:
# apply(check_boards, 3, rowSums)
# apply(check_boards, 3, colSums)

# Part I
# create empty boards of 1s to mark as 0 if get a matching number
# User 0s first of course, but 1s make more sense as can then multiply with the
# original board to get unmarked values
check_boards = array(1, dim = c(5, 5, n_boards))

for (number in numbers){
  #print(number)
  check_boards[boards == number] = 0
  if (any(apply(check_boards, 3, rowSums) == 0 | apply(check_boards, 3, colSums) == 0)){
    print("Winning number:")
    print(number)
    break()
  }
}
# one of these elements is empty so we'll be left with a single number
board_n = c(
  ceiling(which(apply(check_boards, 3, rowSums) == 0)/5),
  ceiling(which(apply(check_boards, 3, colSums) == 0)/5)
)

number*((boards*check_boards)[, , board_n] %>% sum())

# Part II
check_boards = array(1, dim = c(5, 5, n_boards))

for (number in numbers){
  print(number)
  check_boards[boards == number] = 0
  # endgame - if we have the last board we still need to play until it wins
  if (length(dim(boards)) == 2){ # R drops the third dimension if a single board is left
    if (any(rowSums(check_boards) == 0 | colSums(check_boards) == 0)){
      print("Winning number:")
      print(number)
      break()
    }
  } else if (any(apply(check_boards, 3, rowSums) == 0 | apply(check_boards, 3, colSums) == 0)){
    
    board_n = c(
      ceiling(which(apply(check_boards, 3, rowSums) == 0)/5),
      ceiling(which(apply(check_boards, 3, colSums) == 0)/5)
    )
    boards       = boards[, , -board_n]
    check_boards = check_boards[, , -board_n]
    
  }
  
}

number*(boards*check_boards) %>% sum()
