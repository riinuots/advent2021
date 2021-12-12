library(tidyverse)
library(igraph)

input_orig = read_delim("solutions/day12/input", delim = "-", col_names = c("from", "to"))

caves = input_orig %>% 
  graph_from_data_frame(directed = FALSE)
plot(caves)

cave_names = names(V(caves))
n_max = if_else(toupper(cave_names) == cave_names, Inf, 1)
names(n_max) = cave_names

# Part I
cave_names = names(V(caves))
n_max = if_else(toupper(cave_names) == cave_names, Inf, 1)
names(n_max) = cave_names
n_paths = 0
n_end = which(cave_names == "end")

walk = function(current){
  if (current == n_end){return(n_paths <<- n_paths + 1)}
  
  if (n_max[current] == 0){
    return(n_paths)
  }
  
  n_max[current] <<- n_max[current] - 1
  for (nb in neighbors(caves, current)){
    walk(nb)
  }
  n_max[current] <<- n_max[current] + 1
  
}
walk("start")

# Part II
walk_paths = function(current, path = NULL){
  path = paste(path, current, sep = "-")
  # print(path)
  # print(current)
  # 
  # print("n_max:")
  # print(n_max[current])
  
  if (current == n_end){return(paths <<- c(paths, path))}
  
  if (n_max[current] == 0){
    return()
  }
  
  n_max[current] <<- n_max[current] - 1
  for (nb in neighbors(caves, current)){
    #print(nb)
    walk_paths(nb, path)
  }
  n_max[current] <<- n_max[current] + 1
  path = NULL
}
paths = character()
n_max = if_else(toupper(cave_names) == cave_names, Inf, 1)
names(n_max) = cave_names
walk_paths("start")
paths %>% 
  n_distinct()

paths = character()
for (cave in cave_names){
  print(cave)
  if(toupper(cave) == cave | cave %in% c("start", "end")){next}
  n_max = if_else(toupper(cave_names) == cave_names, Inf, 1)
  names(n_max) = cave_names
  n_max[cave] = 2
  walk_paths("start")
  paths = unique(paths)
}

paths %>% 
  n_distinct()

