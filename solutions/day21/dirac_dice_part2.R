library(tidyverse)
library(gtools)

dice_sums  = permutations(3, 3, repeats.allowed = TRUE) %>% rowSums()

p = c(4, 8, 0, 0)
p[1] # player 1 position
p[2] # player 2 position
p[3] # player 1 score
p[4] # player 2 score

wins = c(0, 0)

game = function(p, turn){
  # print("--NEW ROUND--")
  # print("player turn:")
  # print(turn)
  # 
  # print("player position:")
  # print(p[turn])
  # 
  # print("player score:")
  # print(p[turn + 2])

  if (p[turn + 2] > 20){
    #print("-----WINNER----")
    wins[turn] <<- wins[turn] + 1
    p = c(4, 8, 0, 0)
    turn = 1
    return()
  }
  
  for (dice in dice_sums){
    move = p[turn] + dice
    # print("move:")
    # print(move)
    p[turn] = if_else(move %% 10 == 0, 10, move %% 10) # position
    p[turn + 2] = p[turn + 2] + p[turn] # score
    
    if (turn == 1){
      turn = 2
    } else {
      turn = 1
    }

    game(p, turn)
  }

}

game(p, 1)
