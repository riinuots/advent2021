library(tidyverse)

input_orig = read_delim("solutions/day14/input", delim = " -> ",
                        skip = 2, col_names = c("pair", "elem"))
template = read_lines("solutions/day14/input", n_max = 1)

# Part I and II
rules = input_orig %>% 
  separate(pair, into = c("p1", "p2"), sep = 1, remove = FALSE) %>% 
  mutate(insert2 = paste0(elem, p2),
         insert1 = paste0(p1, elem), 
         insert = paste0(p1, elem, p2))

change_n = tibble(name = c("pair", "insert1", "insert2"), dn = c(-1, 1, 1))

all_rules = rules %>% 
  select(pair, insert1, insert2) %>% 
  pivot_longer(-pair, values_to = "new") %>% 
  select(-name)

all_pairs = tibble(pair = rules$pair, n = 0) %>% 
  mutate(n = str_count(template, pair))

for (i in 1:40){
  print(i)
  
  current_pairs = filter(all_pairs)
  current_rules = filter(all_rules, pair %in% current_pairs$pair)
  changes = left_join(current_rules, current_pairs)
  add_new = changes %>% 
    group_by(new) %>% 
    summarise(dn = sum(n)) %>% 
    rename(pair = new)
  remove_old = changes%>% 
    group_by(pair)%>% 
    summarise(dn = -0.5*sum(n))
  
  changes_n = bind_rows(add_new, remove_old) %>% 
    group_by(pair) %>% 
    summarise(dn = sum(dn)) %>% 
    mutate(dn = replace_na(dn, 0))
  
  all_pairs = full_join(all_pairs, changes_n) %>% 
    mutate(dn = replace_na(dn, 0)) %>% 
    mutate(n = n + dn) %>% 
    select(-dn)
  
} %>% 
  suppressMessages()

options(digits = 22)
all_pairs %>% 
  separate(pair, into = c("el1", "el2"), sep = 1) %>% 
  pivot_longer(-n) %>% 
  group_by(value) %>% 
  summarise(n = sum(n)/2) %>% 
  summarise(nmax = max(n), nmin = min(n)) %>% 
  mutate(result = nmax - nmin) %>% 
  pull(result) %>% 
  ceiling()

