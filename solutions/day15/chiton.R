library(tidyverse)
library(igraph)
n = read_lines("solutions/day15/input_test", n_max = 1)
str_count(n)
input_orig = read.fwf("solutions/day15/input", rep(1, 100))

cave_orig = input_orig %>% 
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

# Part I

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


distances(cave, "1-1", "100-100", mode = "in")

# Testing, plotting, etc desperation

p = cave_joined %>% 
  separate(id_nb, into = c("x", "y"), sep = "-", convert = TRUE) %>% 
  distinct(x, y, weight) %>% 
  ggplot(aes(x, y, label = weight)) +
  geom_point(size = 0.5, alpha = 0.2) +
  theme_void()

#p + coord_cartesian(xlim = c(90, 100), ylim = c(90, 100))

cave_joined %>% 
  filter(id_nb == "100-93" | id == "100-93")

cave_test = cave_joined %>% 
  select(id, id_nb, weight, everything()) %>% 
  #distinct(x, y, weight) %>% 
  filter(x > 94, y > 91, y < 95, x > 98) %>% 
  graph_from_data_frame(directed = FALSE)

plot(cave_test)
#plot(cave)

distances(cave, "1-1", "100-100", mode = "in")
# 355 too low
# 369 too high, 368 too high
# 362 not right

path = shortest_paths(cave, "1-1", "100-100", output = "both")
goto = path$vpath[[1]] %>% as_ids()
goto = goto[-1]

# all_paths = cave %>% 
#   all_simple_paths(from = "1-1", to = "10-10") 

mypath = tibble(id = goto) %>% 
  left_join(select(cave_orig, id, weight)) %>% 
  separate(id, into = c("x", "y"), sep = "-", convert = TRUE) %>% 
  distinct()

p1 = p + geom_point(data = mypath, colour = "orange", alpha = 0.4)
p1 + scale_y_reverse()
#p1 + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

# distances() alternative for result:
cave_orig %>% filter(id %in% goto) %>% 
  distinct(id, weight) %>% 
  summarise(sum(weight))

