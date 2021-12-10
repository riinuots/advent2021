library(tidyverse)

input_orig = tibble(line = read_lines("solutions/day10/input"))


pairs = tribble(
  ~open, ~close, ~score,
  "(",     ")",       3,
  "[",     "]",      57,
  "{",     "}",    1197,
  "<",     ">",   25137
) %>% 
  mutate(group = paste0(open, close)) %>% 
  pivot_longer(matches("open|close"), names_to = "type", values_to = "char")

# I could use a loop or I could just copy paste the same line 'enough' times
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

syntax %>% 
  group_by(rowid, group) %>% 
  mutate(n = seq_along(rowid)) %>% 
  filter(type == "close") %>% 
  group_by(rowid) %>% 
  slice(1) %>% 
  ungroup() %>% 
  summarise(sum(score))
