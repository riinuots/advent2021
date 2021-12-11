library(tidyverse)

len = 10
input_orig = read.fwf("solutions/day11/input", rep(1, len)) %>% 
  as.matrix()

crabs = input_orig %>% 
  rbind(rep(-99999, len), ., rep(-99999, len)) %>% 
  cbind(rep(-99999, len+2), ., rep(-99999, len+2))
# neigbours
adj = crossing(di = -1:1, dj = -1:1) %>% 
  filter(! (di == 0 & dj == 0)) %>% 
  as.matrix()

shined = matrix(1, len+2, len+2) # 0 if has shined this round
flashed = 0
#image(shined)
#crabs
n_crabs = len*len
for (iter in 1:500){
  print(iter)
  # start steps
  crabs = crabs + 1
  
  repeat{
    shining = which(crabs >= 10, arr.ind = TRUE) %>% as.matrix()
    shined[shining] = 0
    n_shiners = nrow(shining)
    flashed = flashed + n_shiners
    if (n_shiners == 0){
      if (sum(shined==0) == n_crabs){
        stop("found it!")
      }
      crabs = crabs*shined
      shined = matrix(1, len+2, len+2)
      break
    }
    for (shiner in 1:n_shiners){
      #shiner = 1
      loc = shining[shiner,]
      i = loc["row"]
      j = loc["col"]
      nbs = matrix(c(adj[,"di"] + i, adj[,"dj"] + j), 8, 2)
      crabs[nbs] = crabs[nbs] + 1
      crabs[i, j] = -10 # that'll stop them from reshining
    }
    
  }
}
