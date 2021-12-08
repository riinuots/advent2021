library(tidyverse)

displays_orig = read_delim("solutions/day08/input_test1", delim = " | ", col_names = c("signal", "output"))

# Part I
displays_orig %>% 
  separate_rows(output, sep = " ") %>% 
  mutate(output_n = str_count(output)) %>% 
  count(output_n %in% c(2, 3, 4, 7))

# Part II
library(stringdist)
signal_lengths = tribble(
  ~signal_n, ~value,
  2,   1,
  3,   7,
  4,   4,
  7,   8
)

displays = displays_orig %>% 
  rowid_to_column() %>% 
  filter(rowid == 1) %>% 
  select(-output) %>% 
  separate_rows(signal, sep = " ") %>% 
  mutate(signal_n = str_count(signal)) %>% 
  mutate(signal_ordered = signal %>% 
           str_split("") %>% 
           map(str_sort) %>% 
           map_chr(paste0, collapse = ""))

known = displays %>% 
  left_join(
    tribble(
      ~signal_n, ~value,
      2,   1,
      3,   7,
      4,   4,
      7,   8
    )) %>% 
  drop_na() %>% 
  select(signal, signal_ordered, value)

known_vec = rep(NA, 10)
known_vec[known$value] = known$signal_ordered


# find 3 based on 1
# find 9 based on 3
# find 5 based on 9
# 5 and 0 similar to 9, using 10 for 0

most_similar = tribble(
  ~first, ~second,
  1,   3,
  3,   9,
  9, c(5, 10) # 10 means 0
  
)

match_set = function(df){
  for (i in 1:3){
    #i = 3
    template = most_similar$first[i]
    match    = unlist(most_similar$second[i])
    print(template)
    print(match)
    
    found = displays %>% 
      left_join(known) %>% 
      filter(is.na(value)) %>% 
      mutate(signal_split = signal %>% 
               str_split("") %>% 
               map(str_sort) %>% 
               map_chr(paste0, collapse = "")) %>% 
      mutate(dist = stringdist(signal_split, known_vec[template])) %>% 
      arrange(dist) %>% 
      slice_min(dist) %>% 
      arrange(signal_n) %>% 
      mutate(value = match) %>%
      select(signal, signal_ordered, value)
    
    known = bind_rows(known, found)
    known_vec = rep(NA, 10)
    known_vec[known$value] = known$signal_ordered
  }
  
  # then just 2 and 6 left, identify them based on n characters in signal
  lookup = displays %>% 
    left_join(known) %>% 
    mutate(value = case_when(!is.na(value) ~ value,
                             signal_n == 5 ~ 2,
                             signal_n == 6 ~ 6,
                             TRUE ~ 999))
}