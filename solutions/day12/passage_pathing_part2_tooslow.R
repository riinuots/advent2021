library(tidyverse)
library(igraph)

# works for the tests but too slow for my input data

input_orig = read_delim("solutions/day12/input", delim = "-", col_names = c("from", "to"))

bigs = input_orig %>% 
  filter(to == toupper(to) |
           from == toupper(from)) %>% 
  nest(data = everything())

smalls = input_orig %>% 
  filter(to == tolower(to) & ! (to %in% c("start", "end")) |
           from == tolower(from) & ! (from %in% c("start", "end"))) %>% 
  nest(data = everything())

bigs_repped = bind_rows(replicate(4, bigs, simplify = FALSE)) %>% 
  rowid_to_column("rep_id") %>% 
  unnest(data) %>% 
  group_by(rep_id) %>% 
  mutate(edge_id = row_number()) %>% 
  ungroup() %>% 
  pivot_longer(matches("from|to")) %>% 
  mutate(value = if_else(tolower(value) == value, value, paste0(value, rep_id))) %>% 
  pivot_wider(names_from = "name", values_from = "value") %>% 
  select(-contains("id"))

caves_df = input_orig %>% 
  bind_rows(bigs_repped) 

# smalls = input_orig %>% 
#   bind_rows(unnest(bigs, data)) %>% # include the original bigs here too
#   bind_rows(bigs_repped) %>% 
#   select(-rep_id, -edge_id) %>% 
#   filter(to == tolower(to) |
#            from == tolower(from)) %>% 
#   nest(data = everything())
# 
# smalls_repped = rep_vertices(smalls, 1, big = FALSE)
# 
# caves_df = input_orig %>% 
#   bind_rows(smalls_repped)


#graph_from_data_frame(directed = FALSE)

smalls = input_orig %>% 
  pivot_longer(matches("to|from")) %>% 
  distinct(value) %>% 
  filter(! value %in% c("start", "end")) %>% 
  filter(toupper(value) != value) %>% 
  pull(value)

start_time = Sys.time()
path_n = 0
found_paths = tibble(path = character())
for (mysmall in smalls){
    small_repped = caves_df %>% 
    filter(to == mysmall | from == mysmall) %>% 
    mutate(from = if_else(from == mysmall, paste0(mysmall, "1"), from)) %>% 
    mutate(to   = if_else(to   == mysmall, paste0(mysmall, "1"), to)) 
  
  tempcave = caves_df %>%
    bind_rows(small_repped) %>% 
    graph_from_data_frame(directed = FALSE)
  
  #plot(tempcave)
  
  all_paths = tibble(path = tempcave %>% 
                       all_simple_paths(from = "start", to = "end") %>% 
                       paste0())
  n_found = all_paths$path %>%
    str_remove_all("[:digit:]") %>%
    n_distinct()
  path_n = path_n + n_found

  # found_paths = all_paths %>%
  #   mutate(path = str_remove_all(path, "[:digit:]|\\=|\\(|\\)|^c") %>%
  #            str_squish() %>%
  #            str_remove_all(" ")) %>%
  #   distinct(path) %>%
  #   bind_rows(found_paths)
}

Sys.time() - start_time
# found_paths %>% 
#   distinct() %>% 
#   nrow()

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

# test = found_paths %>% 
#   distinct() %>% 
#   mutate(found = TRUE) %>% 
#   full_join(example_paths)
