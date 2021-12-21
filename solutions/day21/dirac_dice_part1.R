library(tidyverse)

input = unglue::unglue_data(read_lines("solutions/day21/input"),
                            "{player} starting position: {start}",
                            convert = TRUE)

# Part I
game = tibble(die    = rep(1:100, 15),
              player = rep(c(rep("Player 1", 3), rep("Player 2", 3)), 250)) %>% 
  rowid_to_column("n_rolls") %>% 
  mutate(dice = die + lag(die, 1, default = 0) + lag(die, 2, default = 0)) %>% 
  filter(n_rolls %% 3 == 0) %>% 
  select(-die) %>% 
  bind_rows(input, .) %>% 
  group_by(player) %>% 
  fill(start) %>% 
  drop_na() %>% 
  mutate(dice2 = if_else(n_rolls < 7, dice + start, dice),
         move = if_else(cumsum(dice2) %% 10 == 0, 10, cumsum(dice2) %% 10),
         score = cumsum(move))

# grabbing numbers from eyeballing the game tibble:
694*747
