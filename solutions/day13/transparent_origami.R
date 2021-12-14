library(tidyverse)

input_orig = read_csv("solutions/day13/input", col_names = c("x", "y"), n_max = 840)

# Part I
fy = 447
fx = 655

input_orig %>% 
  #mutate(y = if_else(y < fy, y, 2*fy - y))  %>% 
  mutate(x = if_else(x < fx, x, 2*fx - x))  %>% 
  count(x, y) %>% 
  nrow()

# Part II
#input_orig = read_csv("solutions/day13/input_test", col_names = c("x", "y"), n_max = 18)
instructions = unglue::unglue_data(read_lines("solutions/day13/input", skip = 841),
                                   "fold along {var}={value}",
                                   convert = TRUE)

fold = function(df, var, value){
  mutate_at(df, var, ~if_else(.x < value, .x, 2*value - .x))
}

df = input_orig
for (i in 1:nrow(instructions)){
  df = fold(df, instructions$var[i], instructions$value[i])
}

df %>% 
  ggplot(aes(x, y)) +
  geom_tile() +
  coord_fixed() +
  scale_y_reverse()
