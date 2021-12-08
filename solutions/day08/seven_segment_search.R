library(tidyverse)

displays_orig = read_delim("solutions/day08/input", delim = " | ", col_names = c("signal", "output"))

# Part I ----
displays_orig %>% 
  separate_rows(output, sep = " ") %>% 
  mutate(output_n = str_count(output)) %>% 
  count(output_n %in% c(2, 3, 4, 7))

# Part II ----
signal_lengths = tribble(
  ~signal_n, ~value,
  2,   1,
  3,   7,
  4,   4,
  7,   8
)


displays = displays_orig %>% 
  rowid_to_column() %>% 
  #filter(rowid == 1) %>% 
  select(-output) %>% 
  separate_rows(signal, sep = " ") %>% 
  mutate(signal_n = str_count(signal)) %>% 
  mutate(signal_ordered = signal %>% 
           str_split("") %>% 
           map(str_sort) %>% 
           map_chr(paste0, collapse = ""))



# function ----
match_set = function(df){
    
    
  # setup ----
  # browser()
  # df = displays %>% filter(rowid == 6) 
  
  #print("here 1")
  
  known = df %>% 
    left_join(signal_lengths) %>% 
    drop_na() %>% 
    select(signal, signal_ordered, value)
  
  #print("here 2")
  known_vec = rep(NA, 10)
  known_vec[known$value] = known$signal_ordered
  
  
  # identify 3 by comparing to 1 ----
  found = df %>% 
    left_join(known) %>% 
    filter(is.na(value)) %>% 
    mutate(dist = stringdist::stringdist(signal_ordered, known_vec[1])) %>% 
    arrange(dist) %>% 
    slice_min(dist) %>% 
    arrange(signal_n) %>% 
    mutate(value = 3) %>%
    select(signal, signal_ordered, value)
  
  #print("here 3")
  known = bind_rows(known, found)
  known_vec = rep(NA, 10)
  known_vec[known$value] = known$signal_ordered
  
  # 0, 6 and 9 have signal_n = 6
  # comparing to 5 doesn't work
  # 6 is most different to 1
  found = df %>% 
    left_join(known) %>% 
    filter(is.na(value)) %>% 
    filter(signal_n == 6) %>% 
    mutate(dist = stringdist::stringdist(signal_ordered, known_vec[1])) %>% 
    arrange(dist) %>% 
    slice_max(dist) %>% 
    arrange(signal_n) %>% 
    mutate(value = 6) %>%
    select(signal, signal_ordered, value)
  
  known = bind_rows(known, found)
  known_vec = rep(NA, 10)
  known_vec[known$value] = known$signal_ordered
  
  # signal_n = 6: 0 and 9 left. comparing to 3, 9 more similar to 3 than 6
  found = df %>% 
    left_join(known) %>% 
    filter(is.na(value)) %>% 
    filter(signal_n == 6) %>% 
    mutate(dist = stringdist::stringdist(signal_ordered, known_vec[3])) %>% 
    arrange(dist) %>% 
    mutate(value = c(9, 0)) %>% # using 10 as 0 is not a valid index
    select(signal, signal_ordered, value)
  
  known = bind_rows(known, found)
  known_vec = rep(NA, 10)
  known_vec[known$value] = known$signal_ordered
  
  # 2 and 5 left, 5 more similar to 6 than 2
  
  df %>% 
    left_join(known) %>% 
    filter(is.na(value)) %>% 
    mutate(dist = stringdist::stringdist(signal_ordered, known_vec[6])) %>% 
    arrange(dist)
  
  found = df %>% 
    left_join(known) %>% 
    filter(is.na(value)) %>% 
    filter(signal_n == 5) %>% 
    mutate(dist = stringdist::stringdist(signal_ordered, known_vec[6])) %>% 
    arrange(dist) %>% 
    arrange(signal_n) %>% 
    mutate(value = c(5, 2)) %>%
    select(signal, signal_ordered, value)
  
  known = bind_rows(known, found)
  known_vec = rep(NA, 10)
  known_vec[known$value] = known$signal_ordered
  
  
  lookup = df %>% 
    left_join(known) %>% 
    mutate(value = if_else(value == 10, 0, value)) %>% 
    select(-rowid)
  
  return(lookup)
} %>% 
  suppressMessages()

# displays %>% 
#   filter(rowid ==10) %>%  
#   match_set()

outputs = displays_orig %>%
  select(signal = output) %>%
  rowid_to_column() %>%
  separate_rows(signal, sep = " ") %>%
  mutate(signal_ordered = signal %>%
           str_split("") %>%
           map(str_sort) %>%
           map_chr(paste0, collapse = ""))

lookups = displays %>%
  mutate(dummy_rowid = rowid) %>% 
  group_by(dummy_rowid) %>%
  nest() %>%
  mutate(lookup = map(data, match_set)) %>%
  select(rowid = dummy_rowid, lookup) %>%
  unnest(lookup) %>%
  select(rowid, signal_ordered, value)


outputs %>%
  left_join(lookups) %>%
  group_by(rowid) %>%
  summarise(output_value = paste0(value, collapse = "")) %>% 
  summarise(sum(as.numeric(output_value)))



