library(tidyverse)
library(lazyeval)

# Explode ----
explode = function(lst){
  num_df = reshape2::melt(lst) %>% 
    rowid_to_column()
  e = drop_na(num_df) %>% slice(1:2)
  e_row = e$rowid[1] %>% as.numeric()
  # nb - neighbour
  index_cols = names(num_df) %>% str_subset("^L") %>% sort()
  nb1_loc = slice(num_df, e_row-1)[index_cols] %>% as.numeric() %>% na.omit()
  nb2_loc = slice(num_df, e_row+2)[index_cols] %>% as.numeric() %>% na.omit()
  
  # e1 and e2 numbers that are exploding
  e1_loc = slice(e, 1)[index_cols] %>% as.numeric()
  e2_loc = slice(e, 2)[index_cols] %>% as.numeric()
  
  if (! is_empty(nb1_loc)){
    lst[[nb1_loc]] = lst[[nb1_loc]] + unlist(lst[[e1_loc]])
  }
  if (! is_empty(nb2_loc)){
    lst[[nb2_loc]] = lst[[nb2_loc]] + unlist(lst[[e2_loc]])
  }

    # head(x, -1) removes last element
  lst[[head(e1_loc, -1)]] = 0
  return(lst)
}

# Split ----
split = function(lst){
  #lst = num_list
  #unlist(lst)
  num_df = reshape2::melt(lst) %>% 
    rowid_to_column()
  index_cols = names(num_df) %>% str_subset("^L") %>% sort()
  
  # s - number to be split and its indexes
  s = filter(num_df, value > 9) %>% slice(1)
  s_loc = s[index_cols] %>% as.numeric() %>% na.omit()
  
  s1 = floor(s$value/2)
  s2 = ceiling(s$value/2)
  
  lst[[s_loc]] = list(s1, s2)
  return(lst)
}

# Add ----
add = function(lst, new){
  new_list = new %>% 
    str_replace_all("\\[", "list\\(") %>% 
    str_replace_all("\\]", "\\)") %>% 
    lazy_eval()
  
  lst = list(lst, new_list)
  print(unlist(lst))
  # using its df form to access list indexes
  num_df = reshape2::melt(lst)
  
  while(!is.null(try(num_df$L5)) | any(unlist(lst) > 9)){
    if (!is.null(try(num_df$L5))){
      lst = explode(lst)
      num_df = reshape2::melt(lst)
      #print(unlist(lst))
      next
    }
    if (any(unlist(lst) > 9)){
      lst = split(lst)
      num_df = reshape2::melt(lst)
      #print(unlist(lst))
    }
  }
  return(lst)
}

num_list = read_lines("solutions/day18/input", n_max = 1) %>% 
    str_replace_all("\\[", "list\\(") %>%
    str_replace_all("\\]", "\\)") %>%
    lazy_eval()
num_list %>% unlist()

added = num_list
for (i in 1:99){
  new = read_lines("solutions/day18/input", n_max = 1, skip = i)
  print("Adding:")
  print(unlist(new))
  added = add(added, new)
  #added %>% unlist()
}
added %>% unlist()
save(added, file = "solutions/day18/added.rda")
