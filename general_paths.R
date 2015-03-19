load('secor.sb.rda')
library(dplyr)

## Fish leaving Chesapeake Bay
bay.escapes <- secor.sb %>% 
  filter(array %in% c('C&D', 'DE Coast', 'Delaware', 'Long Island', 'Mass',
                      'New Jersey')) 
bay.escapes <- levels(factor(bay.escapes$trans.num))
bay.escapes <- secor.sb %>% 
  filter(trans.num %in% bay.escapes) %>%
  arrange(date.local) %>% 
  data.frame

bay.escapes <- split(bay.escapes, bay.escapes$trans.num)

# Create a character vector with in the order of visted arrays
gen.movement <- function(data){
  if(dim(data)[1] <= 1){
    track <- data[1, 'array']
  }
  else{
      track <- data[1, 'array']
      for(i in seq(1, dim(data)[1] - 1)){
        if(data[i, 'array'] != data[i + 1, 'array']){
          track <- c(track, data[i + 1, 'array'])
        }
      }
  }
  track
}

lapply(bay.escapes, gen.movement)


## Fish staying in the Potomac River
pot.escapes <- secor.sb %>% 
  filter(!array %in% c('Lower Potomac', 'Mid Potomac', 'Upper Potomac'))
pot.escapes <- levels(factor(pot.escapes$trans.num))
pot.residents <- secor.sb %>% 
  filter(!trans.num %in% pot.escapes) %>%
  arrange(date.local) %>% 
  data.frame

pot.residents <- split(pot.residents, pot.residents$trans.num)

lapply(pot.residents, gen.movement)


## Fish staying within the Chesapeake Bay
escapes <- secor.sb %>% 
  filter(array %in% c('C&D', 'DE Coast', 'Delaware', 'Long Island', 'Mass',
                      'New Jersey'))
escapes <- levels(factor(escapes$trans.num))
bay.residents <- secor.sb %>% 
  filter(!trans.num %in% escapes,
         array %in% c('James', 'Lower MD Bay', 'Mid MD Bay', 'Other', 'Patuxent',
                      'Rappahannock', 'Upper MD Bay', 'York'))
bay.residents <- levels(factor(bay.residents$trans.num))
bay.residents <- secor.sb %>%
  filter(trans.num %in% bay.residents) %>% 
  arrange(date.local) %>% 
  data.frame

bay.residents <- split(bay.residents, bay.residents$trans.num)

lapply(bay.residents, gen.movement)
