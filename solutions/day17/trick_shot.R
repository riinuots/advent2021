library(tidyverse)

# tar_x = c(20, 30)
# tar_y = c(-10, -5)

tar_x = c(288, 330)
tar_y = c(-96, -50)
area = tibble(xmin = 288, xmax = 330, ymin = -96, ymax = -50)

do_traj = function(x, y, testing = FALSE){
  loc_x = 0
  loc_y = 0
  x_traj = 0
  y_traj = 0
  for (t in 1:200){
    x_traj = c(x_traj, loc_x + x)
    y_traj = c(y_traj, loc_y + y)
    loc_x = loc_x + x
    loc_y = loc_y + y
    if (loc_x > tar_x[2] | loc_y < tar_y[1]){
      break
    }
    x = case_when(x == 0 ~ 0,
                  x > 0  ~ x - 1,
                  x < 0  ~ x + 1,
                  TRUE ~ 99999)
    y = y -1
    
  }
  if (testing){
    return(tibble(x_traj, y_traj))
  } else if (any(x_traj >= tar_x[1] & x_traj <= tar_x[2] & y_traj >= tar_y[1] & y_traj <= tar_y[2])){
    return(max(y_traj))
  } else{
    return(NA_integer_)
  }
}

# Part I
# see testing.R for visually guesstimating these ranges
crossing(x = 20:30, y = 90:100) %>% 
  rowwise() %>% 
  mutate(max_y = do_traj(x, y)) %>% 
  ungroup() %>% 
  slice_max(max_y)


# Part II
# made the function slightly more efficient, then just iterating over a HUGE range
paths = crossing(x = -100:330, y = -96:100) %>% 
  rowwise() %>% 
  mutate(max_y = do_traj(x, y)) %>% 
  ungroup() 

paths %>% 
  count(!is.na(max_y))
