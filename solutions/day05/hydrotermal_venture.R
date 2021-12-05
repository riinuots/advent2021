library(tidyverse)
theme_set(theme_bw())

# Used the "Import Dataset" interface for most of this:
vents_orig = read_delim("solutions/day05/input", 
                    delim = "->",
                    escape_double = FALSE,
                    col_names =  c("x1y1", "x2y2"), 
                    col_types = cols(x1y1 = col_character(), 
                                     x2y2 = col_character()),
                    trim_ws = TRUE)

vents = vents_orig %>% 
  rowid_to_column() %>% 
  separate(x1y1, into = c("x1", "y1"), sep = ",", convert = TRUE) %>% 
  separate(x2y2, into = c("x2", "y2"), sep = ",", convert = TRUE)

# Part I

# Plotting it:
vents %>% 
  filter(x1 == x2 | y1 == y2) %>% 
  ggplot() +
  geom_linerange(aes(x1, x2, ymin = y1, ymax = y2), size = 3, alpha = 0.3) +
  geom_linerange(aes(y1, y2, xmin = x1, xmax = x2), size = 3, alpha = 0.3) +
  expand_limits(x = 0, y = 0) +
  scale_y_reverse() +
  coord_fixed()
ggsave("solutions/day05/vents.png", width = 5, height = 5)

vents %>% 
  filter(x1 == x2 | y1 == y2) %>% 
  # it's all single rows, but this group_by() is a 'hack' to get a non-vectorised function
  # - seq() - to work as a vectorized one
  group_by(rowid) %>% 
  mutate(x = paste0(seq(x1, x2), collapse = ","),
         y = paste0(seq(y1, y2), collapse = ",")) %>% 
  ungroup() %>% 
  separate_rows(x, y) %>% 
  count(x, y) %>% 
  count(n > 1)

# Part II - code identical to Part I, only removed filter(x1 == x2 | y1 == y2)

# Plotting it:
vents %>% 
  ggplot() +
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), size = 3, alpha = 0.1) +
  expand_limits(x = 0, y = 0) +
  scale_y_reverse() +
  coord_fixed()

vents %>% 
  # it's all single rows, but this group_by() is a 'hack' to get a non-vectorised function
  # - seq() - to work as a vectorized one
  group_by(rowid) %>% 
  mutate(x = paste0(seq(x1, x2), collapse = ","),
         y = paste0(seq(y1, y2), collapse = ",")) %>% 
  ungroup() %>% 
  separate_rows(x, y) %>% 
  count(x, y) %>% 
  count(n > 1)

