load('secor.sb.rda')
library(TelemetryR); library(dplyr)

## Fish leaving Chesapeake Bay
bay.escapes <- secor.sb %>%
  # fish that have been detected in coastal waters
  filter(array %in% c('Bay Mouth', 'C&D', 'DE Coast', 'Delaware', 'Hudson',
                      'Long Island', 'Mass', 'MD Coast', 'New Jersey', 'NYB',
                      'VA Coast')) %>% 
  distinct(transmitter) %>% 
  # select detections of those transmitters
  left_join(secor.sb) %>%
  arrange(date.local) %>%
  # split using base R
  data.frame %>% 
  split(.$transmitter)

# Create a character vector with in the order of visted arrays
library(parallel)

cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, library(TelemetryR))
escape.tracks <- parLapply(cl, bay.escapes, track, dates = 'date.local',
                           ids = 'array')


paste('Number that left =', length(escape.tracks))
paste('Number that got to Mass. =', length(grep('Mass', escape.tracks)))
paste('Number that got to Long Island =',
      length(intersect(grep('Mass', escape.tracks, invert = T),
                       grep('Island', escape.tracks))))
paste('Number that got to Delaware =',
      length(intersect(grep('Island', escape.tracks, invert = T),
                       grep('De', escape.tracks))))

## Fish staying in the Potomac River
pot.residents <- secor.sb %>%
  filter(!array %in% c('Lower Potomac', 'Mid Potomac', 'Upper Potomac')) %>% 
  distinct(transmitter) %>% 
  # Drop transmitters that have detections outside of the Potomac
  anti_join(secor.sb, .) %>%
  arrange(date.local) %>%
  # Split using base R
  data.frame %>% 
  split(.$transmitter)

parLapply(cl, pot.residents, track, dates = 'date.local',
          ids = 'array')


## Fish staying within the Chesapeake Bay
bay.residents <- secor.sb %>%
  filter(array %in% c('Bay Mouth', 'C&D', 'DE Coast', 'Delaware', 'Long Island',
                      'Mass', 'MD Coast', 'New Jersey', 'VA Coast')) %>% 
  distinct(transmitter) %>% 
  anti_join(secor.sb, .) %>% 
  distinct(transmitter) %>% 
  left_join(secor.sb) %>% 
  arrange(date.local) %>%
  data.frame %>% 
  split(.$transmitter)

parLapply(cl, bay.residents, track, dates = 'date.local',
          ids = 'array')

stopCluster(cl)
