library(tidyverse)
library(igraph)

input_orig = read_delim("solutions/day12/input_test1", delim = "-", col_names = c("from", "to"))

bigs = input_orig %>% 
  filter(to == toupper(to) |
           from == toupper(from)) %>% 
  nest(data = everything())

smalls = input_orig %>% 
  filter(to == tolower(to) & ! (to %in% c("start", "end")) |
           from == tolower(from) & ! (from %in% c("start", "end"))) %>% 
  nest(data = everything())

rep_vertices = function(df, n, big = TRUE){
  tmp = bind_rows(replicate(n, df, simplify = FALSE)) %>% 
    rowid_to_column("rep_id") %>% 
    unnest(data) %>% 
    group_by(rep_id) %>% 
    mutate(edge_id = row_number()) %>% 
    ungroup() %>% 
    pivot_longer(matches("from|to"))
  
  if (big){
    tmp %>% 
      mutate(value = if_else(tolower(value) == value, value, paste0(value, rep_id))) %>% 
      pivot_wider(names_from = "name", values_from = "value")
  }else{
    tmp %>% 
      mutate(value = if_else(toupper(value) == value | value %in% c("start", "end"), value, paste0(value, rep_id))) %>% 
      pivot_wider(names_from = "name", values_from = "value")
  }
  
}
bigs_repped = rep_vertices(bigs, 4)

smalls = input_orig %>% 
  bind_rows(unnest(bigs, data)) %>% # include the original bigs here too
  bind_rows(bigs_repped) %>% 
  select(-rep_id, -edge_id) %>% 
  filter(to == tolower(to) |
           from == tolower(from)) %>% 
  nest(data = everything())

smalls_repped = rep_vertices(smalls, 1, big = FALSE)

caves_df = input_orig %>% 
  bind_rows(smalls_repped)


#graph_from_data_frame(directed = FALSE)

smalls_names = input_orig %>% 
  pivot_longer(matches("to|from")) %>% 
  distinct(value) %>% 
  filter(! value %in% c("start", "end")) %>% 
  filter(toupper(value) != value) %>% 
  pull(value)

path_n = 0
found_paths = tibble(path = character())
for (mysmall in smalls_names){
  mysmall = "c"
  other_smalls = smalls_names[! smalls_names %in% mysmall] %>% 
    paste0(1)
  
  tempcave = caves_df %>%
    mutate(from = if_else(from %in% other_smalls, str_remove(from, "[:digit:]"), from)) %>% 
    mutate(to   = if_else(to   %in% other_smalls, str_remove(to, "[:digit:]"), to)) %>% 
    graph_from_data_frame(directed = FALSE)
  
  plot(tempcave)
  all_paths = tibble(path = tempcave %>% 
                       all_simple_paths(from = "start", to = "end") %>% 
                       paste0())

  found_paths = all_paths %>% 
    mutate(path = str_remove_all(path, "[:digit:]|\\=|\\(|\\)|^c") %>% 
             str_squish() %>% 
             str_remove_all(" ")) %>% 
    distinct(path) %>% 
    bind_rows(found_paths)
}

found_paths %>% 
  distinct() %>% 
  nrow()

example_paths = tibble(correct = TRUE, path = read_lines("start,A,b,A,b,A,c,A,end
start,A,b,A,b,A,end
start,A,b,A,b,end
start,A,b,A,c,A,b,A,end
start,A,b,A,c,A,b,end
start,A,b,A,c,A,c,A,end
start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,d,b,A,c,A,end
start,A,b,d,b,A,end
start,A,b,d,b,end
start,A,b,end
start,A,c,A,b,A,b,A,end
start,A,c,A,b,A,b,end
start,A,c,A,b,A,c,A,end
start,A,c,A,b,A,end
start,A,c,A,b,d,b,A,end
start,A,c,A,b,d,b,end
start,A,c,A,b,end
start,A,c,A,c,A,b,A,end
start,A,c,A,c,A,b,end
start,A,c,A,c,A,end
start,A,c,A,end
start,A,end
start,b,A,b,A,c,A,end
start,b,A,b,A,end
start,b,A,b,end
start,b,A,c,A,b,A,end
start,b,A,c,A,b,end
start,b,A,c,A,c,A,end
start,b,A,c,A,end
start,b,A,end
start,b,d,b,A,c,A,end
start,b,d,b,A,end
start,b,d,b,end
start,b,end
"))

test = found_paths %>% 
  distinct() %>% 
  mutate(found = TRUE) %>% 
  full_join(example_paths)
