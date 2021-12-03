library(tidyverse)
len = 12
report = tibble(value = scan("solutions/day03/input", what = "character")) %>% 
  separate(value, into = paste0("v_", 1:len), sep = 1:len, convert = TRUE)

# Part I
half = floor(nrow(report)/2)

report %>% 
  summarise_all(sum) %>% 
  pivot_longer(everything()) %>% 
  transmute(first  = as.numeric(value > half),
            second = as.numeric(value < half)) %>% 
  summarise_all(paste0, collapse = "") %>% 
  summarise_all(strtoi, base = 2) %>% 
  mutate(first*second)

# Part II
pop   = function(x){ceiling(median(x))}
unpop = function(x){
  if (mean(x) < 0.5){
    return(1)
  } else{
    return(0)
  }
}

report %>% 
  filter(v_1  == pop(v_1)) %>% 
  filter(v_2  == pop(v_2)) %>% 
  filter(v_3  == pop(v_3)) %>% 
  filter(v_4  == pop(v_4)) %>% 
  filter(v_5  == pop(v_5)) %>% 
  filter(v_6  == pop(v_6)) %>%
  filter(v_7  == pop(v_7)) %>%
  filter(v_8  == pop(v_8)) %>%
  filter(v_9  == pop(v_9)) %>%
  filter(v_10 == pop(v_10)) %>%
  filter(v_11 == pop(v_11)) %>%
  filter(v_12 == pop(v_12)) %>%
  pivot_longer(everything()) %>% 
  summarise_all(paste0, collapse = "") %>% 
  summarise_all(strtoi, base = 2)

report %>% 
  filter(v_1  == unpop(v_1)) %>% 
  filter(v_2  == unpop(v_2)) %>% 
  filter(v_3  == unpop(v_3)) %>% 
  filter(v_4  == unpop(v_4)) %>%
  filter(v_5  == unpop(v_5)) %>%
  filter(v_6  == unpop(v_6)) %>%
  filter(v_7  == unpop(v_7)) %>%
  filter(v_8  == unpop(v_8)) %>%
  filter(v_9  == unpop(v_9)) %>%
  #filter(v_10 == unpop(v_10)) %>%
  #filter(v_11 == unpop(v_11)) %>%
  #filter(v_12 == unpop(v_12)) %>%
  pivot_longer(everything()) %>% 
  summarise_all(paste0, collapse = "") %>% 
  summarise_all(strtoi, base = 2)


2235*451
