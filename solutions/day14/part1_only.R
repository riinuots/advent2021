library(tidyverse)

input_orig = read_delim("solutions/day14/input_test", delim = " -> ",
                        skip = 2, col_names = c("pair", "elem"))
template = read_lines("solutions/day14/input_test", n_max = 1)


# Part I
rules = input_orig %>% 
  separate(pair, into = c("p1", "p2"), sep = 1, remove = FALSE) %>% 
  mutate(insert = paste0(elem, p2))

rules_list = rules$insert
names(rules_list) = rules$pair

pol0 = template
all_pol = character()
for (i in 1:5){
  print(i)
  pol = str_sub(pol0, 1, 1)
  all_pol = c(all_pol, pol0)
  for (loc in 1:(str_length(pol0)-1)){
    p1 = str_sub(pol0, loc, loc)
    p2 = str_sub(pol0, loc+1, loc+1)
    p3 = rules_list[str_sub(pol0, loc, loc+1)]
    pol = paste0(pol, if_else(is.na(p3), p2, p3))
  }
  pol0 = pol
}
tibble(element = LETTERS,
       n = str_count(pol, LETTERS)) %>% 
  filter(n != 0) %>% 
  summarise(nmax = max(n), nmin = min(n)) %>% 
  mutate(result = nmax - nmin)

