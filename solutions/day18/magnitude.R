library(tidyverse)

load("solutions/day18/added.rda")

added_df = added %>% 
  reshape2::melt() %>% 
  as_tibble()

added %>% unlist()
# I'm crafty
first = ((7*3 + 6*2)*3 + (7*3 + 7*2)*2)*3 + ((7*3 + 0*2)*3 + (7*3 + 7*2)*2)*2
second = ((7*3 + 7*2)*3 + (7*3 + 8*2)*2)*3 + ((8*3 + 7*2)*3 + (7*3 + 8*2)*2)*2
first*3 + second*2