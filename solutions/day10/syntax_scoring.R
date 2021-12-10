library(tidyverse)

input_orig = tibble(line = read_lines("solutions/day10/input"))


pairs = tribble(
  ~open, ~close, ~score, ~score2,
  "(",     ")",       3,       1,
  "[",     "]",      57,       2,
  "{",     "}",    1197,       3,
  "<",     ">",   25137,       4    
) %>% 
  mutate(group = paste0(open, close)) %>% 
  pivot_longer(matches("open|close"), names_to = "type", values_to = "char")

# I could use a loop or I could just copy paste the same line 'enough' times
# these str_remove_all() lines take away correctly complete pairs
# leaving us with corrupt or incomplete ones
syntax = input_orig %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  mutate(line = str_remove_all(line, "\\(\\)|\\[\\]|\\{\\}|\\<\\>")) %>% 
  rowid_to_column() %>% 
  separate_rows(line, sep = "") %>% 
  rename(char = line) %>% 
  filter(char != "") %>% 
  left_join(pairs)

faulty = syntax %>% 
  filter(type == "close") %>% 
  group_by(rowid) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(total = sum(score))

# Part II

# did some pen and paper work to figure out the 5^n component
syntax %>% 
  filter(! rowid %in% faulty$rowid) %>% 
  group_by(rowid) %>%
  mutate(n = seq_along(rowid) - 2) %>% 
  mutate(part = score2*(5^n)) %>% 
  mutate(part = if_else(n < 0, 0, part)) %>% 
  mutate(parts = sum(part)*5) %>% 
  slice(1) %>% 
  mutate(total = parts + score2) %>% 
  ungroup() %>% 
  summarise(median(total))

  
