library(tidyverse)
library(unglue)

input_orig = unglue::unglue_data(read_lines("solutions/day22/input_test2"),
                                   "{switch} x={xmin}..{xmax},y={ymin}..{ymax},z={zmin}..{zmax}",
                                 convert = TRUE)

# Part I
input = input_orig %>% 
  rowwise() %>% 
  mutate(x = list(xmin:xmax),
         y = list(ymin:ymax),
         z = list(zmin:zmax)) %>% 
  ungroup() %>% 
  rowid_to_column("id")

x = input %>% 
  select(id, switch, x) %>% 
  unnest(x) %>% 
  group_by(switch) %>% 
  filter(x >= -50, x <= 50)

y = input %>% 
  select(id, switch, y) %>% 
  unnest(y) %>% 
  group_by(switch) %>% 
  filter(y >= -50, y <= 50)

z = input %>% 
  select(id, switch, z) %>% 
  unnest(z) %>% 
  group_by(switch) %>% 
  filter(z >= -50, z <= 50)


init = full_join(x, y, by = c("id", "switch")) %>% 
  full_join(z, by = c("id", "switch")) %>% 
  ungroup() %>% 
  arrange(-id)


init %>% 
  drop_na() %>% 
  distinct(x, y, z, .keep_all = TRUE) %>% 
  count(switch == "on")



