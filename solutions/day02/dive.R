library(tidyverse)
commands = read_delim("solutions/day02/input", col_names = c("command", "value"))

# Part I
moves = tribble(
  ~command,  ~d, ~dir,
  "forward",  1, "hor",
  "down",     1, "vert",
  "up",      -1, "vert"
)

commands %>% 
  left_join(moves) %>% 
  mutate(value = value*d) %>% 
  group_by(dir) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  pull(value) %>% 
  prod()

# Part II
commands = commands %>% 
  left_join(moves) %>% 
  mutate(value = value*d)

aim = 0
x = 0
z = 0
for (i in 1:nrow(commands)){
  print(i)
  dir = commands[i, "dir"]
  value = commands[i, "value"]
  if (dir == "vert"){
    aim = aim + value
  } else if(dir == "hor"){
    x = x + value
    z = z + value*aim
  } else{
    print(i)
    break("unknown command")
  }
}
options(digits = 22)
prod(x, z)


