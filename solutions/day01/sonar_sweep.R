library(tidyverse)
seafloor = tibble(depth = scan("solutions/day01/input"))

# Part I
seafloor %>% 
  mutate(increase = (depth - lag(depth)) > 0) %>% 
  count(increase)

# Part II
seafloor %>% 
  mutate(sliding_sum = depth + lag(depth) + lead(depth)) %>% 
  mutate(increase = (sliding_sum - lag(sliding_sum)) > 0) %>% 
  count(increase)
