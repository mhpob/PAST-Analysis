load('secor.sb.rda')
library(TelemetryR); library(dplyr)

## Fish leaving Chesapeake Bay
bay.escapes <- secor.sb %>%
  filter(array %in% c('Bay Mouth', 'C&D', 'DE Coast', 'Delaware', 'Long Island',
                      'Mass', 'MD Coast', 'New Jersey', 'VA Coast'))
bay.escapes <- unique(bay.escapes$trans.num)
bay.escapes <- secor.sb %>%
  filter(trans.num %in% bay.escapes) %>%
  arrange(date.local) %>%
  data.frame

bay.escapes <- split(bay.escapes, bay.escapes$trans.num)

# Create a character vector with in the order of visted arrays
escape.tracks <- lapply(bay.escapes, track, dates = 'date.local',
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
pot.escapes <- secor.sb %>%
  filter(!array %in% c('Lower Potomac', 'Mid Potomac', 'Upper Potomac'))
pot.escapes <- unique(pot.escapes$trans.num)
pot.residents <- secor.sb %>%
  filter(!trans.num %in% pot.escapes) %>%
  arrange(date.local) %>%
  data.frame

pot.residents <- split(pot.residents, pot.residents$trans.num)

lapply(pot.residents, track, dates = 'date.local',
            ids = 'array')


## Fish staying within the Chesapeake Bay
escapes <- secor.sb %>%
  filter(array %in% c('Bay Mouth', 'C&D', 'DE Coast', 'Delaware', 'Long Island',
                      'Mass', 'MD Coast', 'New Jersey', 'VA Coast'))
escapes <- unique(escapes$trans.num)
bay.residents <- secor.sb %>%
  filter(!trans.num %in% escapes)
bay.residents <- unique(bay.residents$trans.num)
bay.residents <- secor.sb %>%
  filter(trans.num %in% bay.residents) %>%
  arrange(date.local) %>%
  data.frame

bay.residents <- split(bay.residents, bay.residents$trans.num)

lapply(bay.residents, track, dates = 'date.local',
            ids = 'array')
