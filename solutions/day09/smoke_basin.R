library(tidyverse)

input_orig = tibble(tubes = read_lines("solutions/day09/input_test"))
n_cols = input_orig$tubes[1] %>% str_count(".")
n_rows = nrow(input_orig)

tubes = input_orig %>% 
  rowid_to_column("x") %>% 
  separate_rows(tubes, sep = "", convert = TRUE) %>% 
  drop_na() %>% 
  rename(value = tubes) %>% 
  mutate(y = rep(1:n_cols, n_rows)) %>% 
  mutate(id = paste(x, y, sep = "-"))

# Part I
# nbs - neighbours
nbs = tribble(
  ~dx, ~dy,
  -1,  0, 
   0, -1, 
   0,  1, 
   1,  0
)


all_neighbours = tubes %>% 
  crossing(nbs) %>% 
  mutate(x = x + dx,
         y = y + dy) %>% 
  rename(value0 = value) %>% 
  left_join(select(tubes, -id)) %>% 
  drop_na() %>% 
  mutate(id_nb = paste(x, y, sep = "-"))

low_points =  all_neighbours %>% 
  group_by(id, value0) %>% 
  summarise(low_point = all(value0 < value)) %>% 
  filter(low_point) %>% 
  ungroup()


low_points %>% summarise(sum(value0 + 1))

# Part II
library(igraph)

# all_neighbours created in Part I using crossing()
cave = all_neighbours %>%
  filter(value0 != 9, value != 9) %>% 
  select(id, id_nb, value) %>% 
  graph_from_data_frame()

# igraph gives us all basins and their sizes as components
components(cave)$csize %>% 
  sort(decreasing = TRUE) %>% 
  .[1:3] %>% 
  prod()

png("solutions/day09/cave_graph.png")
plot(cave, vertex.color = "white")
dev.off()