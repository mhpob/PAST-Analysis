load('secor.sb.rda')

library(dplyr)
escapes <- secor.sb %>% 
  filter(array %in% c('C&D', 'Delaware', 'Elk', 'Long Island', 'Mass',
                      'New Jersey')) 
escapes <- levels(factor(escapes$trans.num))
escapes <- secor.sb %>% 
  filter(trans.num %in% escapes) %>%
  arrange(date.local) %>% 
  data.frame

escapes <- split(escapes, escapes$trans.num)

k <- function(data){
  l <- data[1]
  for(i in seq(1,length(data)-1)){
    if(data[i] != data[i+1]){
      l <- c(l, data[i+1])
    }
  }
  l
}

k(escapes[['25490']][, 'array'])
j <- escapes[['25490']]
j[1,1]


leavepot <- secor.sb %>% 
  filter(!array %in% c('CBIBS', 'DDOE','Piney Point', 'Rt 301'))
leavepot <- levels(factor(leavepot$trans.num))
res <- secor.sb %>% 
  filter(!trans.num %in% leave) %>%
  arrange(date.local) %>% 
  data.frame

res <- split(res, res$trans.num)

k <- function(data){
  l <- data[1]
  for(i in seq(1,length(data)-1)){
    if(data[i] != data[i+1]){
      l <- c(l, data[i+1])
    }
  }
  l
}

k(res[[1]][, 'station'])
length(res[[1]][,'station'])



escapes <- secor.sb %>% 
  filter(array %in% c('C&D', 'Delaware','Long Island', 'Mass',
                      'New Jersey'))
escapes <- levels(factor(escapes$trans.num))
escapes <- secor.sb %>% 
  filter(!trans.num %in% escapes,
         array %in% c('CBL Pier', 'Cedar Point', 'Elizabeth', 'Elk', 'James',
                      'Kent Island', 'Navy', 'Rappahannock', 'SERC', 'York'))
escapes <- levels(factor(escapes$trans.num))
escapes <- secor.sb %>%
  filter(trans.num %in% escapes) %>% 
  arrange(date.local) %>% 
  data.frame

escapes <- split(escapes, escapes$trans.num)

k <- function(data){
  if(dim(data)[1] <= 1){
    l <- data[1, 'array']
  }
  else{
      l <- data[1, 'array']
      for(i in seq(1,dim(data)[1]-1)){
        if(data[i, 'array'] != data[i+1, 'array']){
          l <- c(l, data[i+1, 'array'])
        }
      }
  }
  l
}

lapply(escapes, k)
