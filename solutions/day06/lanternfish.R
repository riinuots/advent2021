library(tidyverse)

fish_orig = scan("solutions/day06/input", sep = ",")

# Part I
n_new = 0
fish = fish_orig
for (i in 1:80){
  #print(i)
  print(fish)
  
  n_new = sum(fish == 0)
  
  fish = fish - 1
  fish[fish == -1] = 6
  fish = c(fish, rep(8, n_new))
}
length(fish)

# Part II
# Need to keep track of frequencies of fish instead of fish themselves (from @drob on Twitter)

all_fish = tibble(fish = 0:8)

sea_orig = tibble(fish = fish_orig) %>% 
  count(fish) %>% 
  full_join(all_fish) %>% 
  arrange(-fish) %>% 
  mutate(n = replace_na(n, 0))

sea_dummy = tibble(fish = rep(8:0, 256), n = 0)

sea = bind_rows(sea_orig, sea_dummy)
for (i in 1:256){
  print(i)
  sea = sea %>% 
    mutate(n_new = if_else(fish == 6,
                           lag(n, default = 0) + lag(n, 3, default = 0),
                           lag(n, default = 0))) %>% 
    mutate(n = n_new)
}
options(digits = 22)
sea$n %>% 
  sum(na.rm = TRUE)

