library(tidyverse)
library(igraph)

input_orig = read_delim("solutions/day12/input", delim = "-", col_names = c("from", "to"))

bigs = input_orig %>% 
  filter(to == toupper(to) |
           from == toupper(from)) %>% 
  nest(everything())

# I am replicating the big points with slightly different names
# this way the simple paths function will 'visit' them more than onnce
bigs_repped = bind_rows(replicate(4, bigs, simplify = FALSE)) %>% 
  rowid_to_column("rep_id") %>% 
  unnest(data) %>% 
  group_by(rep_id) %>% 
  mutate(edge_id = row_number()) %>% 
  pivot_longer(matches("from|to")) %>% 
  mutate(value = if_else(tolower(value) == value, value, paste0(value, rep_id))) %>% 
  pivot_wider(names_from = "name", values_from = "value")
  
# add my replications into the original, create graph
caves = input_orig %>% 
  bind_rows(bigs_repped) %>% 
  graph_from_data_frame(directed = FALSE)

# find all paths
all_paths = tibble(path = caves %>% 
  all_simple_paths(from = "start", to = "end") %>% 
    paste0())

# remove numbers so replicates look identical to originals
# count distinct
all_paths %>% 
  mutate(path = str_remove_all(path, "[:digit:]")) %>% 
  distinct(path) %>% 
  nrow()

# plots

caves0 = input_orig %>% 
  graph_from_data_frame(directed = FALSE)

caves0 %>% plot()

caves %>% plot()