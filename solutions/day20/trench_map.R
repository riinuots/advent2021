library(tidyverse)

filename = "solutions/day20/input_test"

algo_orig = read_lines(filename, n_max = 1) %>% 
  str_split("") %>% 
  unlist()

algo = tibble(value = algo_orig) %>% 
  rowid_to_column("algo") %>% 
  mutate(algo = algo -1) %>% 
  mutate(value = value %>% 
           str_replace_all("\\.", "0") %>% 
           str_replace_all("#",   "1"))

len0 = str_count(read_lines(filename, skip = 2, n_max = 1), "")
addcols = paste0(rep(".", 200), collapse = "")
addlines = tibble(line = rep(paste0(rep(".", 200+len0+200), collapse = ""), 200))
len = 400+len0

input_orig = tibble(line = read_lines(filename, skip = 2)) %>% 
  mutate(line = paste0(addcols, line, addcols)) %>% 
  bind_rows(addlines) %>% 
  bind_rows(addlines, .) %>% 
  separate(line, into = paste0("v_", 1:len), sep = 1:len) %>% 
  arrange(desc(row_number())) %>% 
  rowid_to_column("y") %>% 
  arrange(-y) %>% 
  pivot_longer(starts_with("v_")) %>% 
  rename(x = name) %>% 
  mutate(x = str_remove(x, "v_") %>% parse_number()) %>% 
  mutate(value = value %>% 
           str_replace_all("\\.", "0") %>% 
           str_replace_all("#",   "1"))

nbs = crossing(dy = -1:1, dx = -1:1)

enhance = function(df, fill = "0"){
  df %>% 
    crossing(nbs) %>% 
    mutate(nx = x+dx,
           ny = y+dy) %>% 
    select(-value) %>% 
    left_join(df, by = c("nx" = "x", "ny" = "y")) %>% 
    mutate(value = if_else(is.na(value), fill, value)) %>% 
    group_by(x, y) %>% 
    arrange(desc(dy)) %>% 
    summarise(algo01 = paste0(value, collapse = ""),
              algo   = strtoi(algo01, base = 2)
    ) %>% 
    ungroup() %>% 
    left_join(algo, by = c("algo" = "algo"))
} %>% suppressMessages()

# Part I
input_orig %>% 
  enhance(fill = "0") %>% 
  enhance(fill = "1") %>% 
  count(value == 1)

# Part II
image = input_orig
for (i in 1:50){
  print(i)
  myfill = "0"
  if (i %% 2 == 0){myfill = "1"}
  image = enhance(image, fill = myfill)
}

image %>% 
  count(value == 1)







# plotting

input_orig %>% 
  enhance(fill = "0") %>% 
  enhance(fill = "0") %>% 
  filter(value == 1) %>% 
  ggplot(aes(x, y)) +
  geom_point(shape = "#", size = 5)
ggsave("solutions/day20/image.png", width = 5, height = 5)
