library(tidyverse)
library(igraph)

len = str_count(read_lines("solutions/day15/input", n_max = 1))
input_orig = read.fwf("solutions/day15/input", rep(1, len))

# Part II
m0 = input_orig %>% as.matrix()

m = rbind(
  cbind(m0,   m0+1, m0+2, m0+3, m0+4),
  cbind(m0+1, m0+2, m0+3, m0+4, m0+5),
  cbind(m0+2, m0+3, m0+4, m0+5, m0+6),
  cbind(m0+3, m0+4, m0+5, m0+6, m0+7),
  cbind(m0+4, m0+5, m0+6, m0+7, m0+8)
) %>% 
  if_else(. > 9, . %% 9, .) %>% 
  matrix(ncol = len*5, byrow = TRUE) %>% 
  as_tibble()


cave_orig = m %>% 
  rowid_to_column("x") %>% 
  pivot_longer(-x, names_to = "y", values_to = "weight") %>% 
  mutate(y = str_remove(y, "V") %>% parse_integer()) %>% 
  mutate(id = paste(x, y, sep = "-"))

nbs = tribble(
  ~dx, ~dy,
  -1,  0, 
  0, -1, 
  0,  1, 
  1,  0
)

# Part II

cave_joined = cave_orig %>% 
  tidyr::crossing(nbs) %>% 
  mutate(x = x + dx,
         y = y + dy) %>% 
  rename(weight0 = weight) %>% 
  left_join(select(cave_orig, -id)) %>% 
  drop_na() %>% 
  mutate(id_nb = paste(x, y, sep = "-"))

cave = cave_joined %>%
  select(id, id_nb, weight) %>%
  graph_from_data_frame()

distances(cave, "1-1", "500-500", mode = "out")


