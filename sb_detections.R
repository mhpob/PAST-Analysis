library(TelemetryR); library(lubridate); library(dplyr)

detects <- vemsort('p:/obrien/biotelemetry/detections')

secor.sb <- detects %>% 
  filter(transmitter %in% paste0('A69-1601-', seq(25434, 25533, 1)) |
         transmitter %in% paste0('A69-1601-', seq(53850, 53899, 1))) %>%
  select(-one_of('trans.name', 'trans.serial', 'sensor.value',
                 'sensor.unit')) %>% 
  data.frame()

arr <- function(part){grepl(part, secor.sb[, 'station'], ignore.case = T)}
  
secor.sb$array <- 
      ifelse(arr('piney|pot|look'), 'Lower Potomac',
      ifelse(arr('301|rad') | secor.sb$station == 'Report1', 'Mid Potomac',
      ifelse(secor.sb$station %in% c('Alexandria', 'Dogue Creek', 'Hains Point',
                  'Mattawoman', 'National Harbor', 'Piscataway', 'Pomonkey',
                  'Roosevelt Br.', 'S. Capitol Br.', 'S. Craney Isl.', 'Report2',
                  'Report8'),
             'Upper Potomac',
      ifelse(arr('cbl|jacks|pax|nott|jug') |
               secor.sb$station %in% c('Benedicts Bridge', 'Broomes',
                   'Magruders', 'Pepco', 'SERC Active'), 'Patuxent',
      ifelse(arr('chop'), 'Choptank',
      ifelse(arr('kent|annap|dock|rho') | secor.sb$station %in% c('Report3',
                                    'Report7', 'Report9'), 'Mid MD Bay',
      ifelse(arr('cedar|goose'), 'Lower MD Bay',
      ifelse(arr('elk|pata') | secor.sb$station == 'Report5', 'Upper MD Bay',
      ifelse(arr('&| 32'), 'C&D',
      ifelse(arr('marsh|nan'), 'Nanticoke',
      ifelse(arr('rapp|sting|cr '), 'Rappahannock',
      ifelse(arr('vims|^y'), 'York',
      ifelse(arr('^nn|(g|^r)\\d|bur| poco|^hi|james') |
               secor.sb$station %in% c('NH8', 'NH10'), 'James',
      ifelse(secor.sb$station %in% c('APM1',
                                     paste0('NH', 12:35)), 'Elizabeth',
      ifelse(arr('v-|t-|a-|cs-|inner|outer|middle'), 'MD Coast',
      ifelse(arr('# 2|# 3'), 'Delaware',
      ifelse(arr('# 1'), 'DE Coast',
      ifelse(arr('sandy|barnegat|shark river'), 'New Jersey',
      ifelse(secor.sb$station == 'Storm King', 'Hudson',
      ifelse(arr('fire|inlet \\d|jones|montauk|rockaway|shinnecock|swg|thames') |
               secor.sb$station == 'Report4', 'Long Island',
      ifelse(arr('dmf|vine|cz|ph|nera|plum|joppa|^er|(ca|bb|bh)\\d|nau|chat|
                 |mono|cove|elli| inl|orl|sci|jer|ccc|vs'),
             'Mass',
      ifelse(arr('(^b|ts)\\d|CBB|LC|henry|cc ls|\\dch|^ch$'),'Bay Mouth',
      ifelse(arr('(^cb|ri)($|\\d)|^nc|^(ra$|rao)|scl|wea'), 'VA Coast',
             'Other')))))))))))))))))))))))

tag.data <- read.csv('p:/obrien/biotelemetry/PAST SB/taggingdata.csv',
                     stringsAsFactors = F)
tag.data$Date <- mdy(tag.data$Date, tz = 'America/New_York')
tag.data <- tag.data[, c('Date', 'Tag.ID', 'Length..TL..mm.',
                         'Weight..kg.', 'Sex', 'Age.Scale')]
names(tag.data) <- c('tag.date', 'transmitter', 'length',
                     'weight', 'sex', 'age')

# we reused tag A69-1601-25465 on 2014-10-30. Need to split tagging data to
# reflect this.
firsttagging.25465 <- secor.sb %>% 
  filter(transmitter == 'A69-1601-25465', date.utc <= '2014-06-15') %>%
  merge(tag.data[32,], all.x = T)

secondtagging.25465 <- secor.sb %>% 
  filter(transmitter == 'A69-1601-25465', date.utc >= '2014-10-29') %>%
  merge(tag.data[101,], all.x = T)

secor.sb <- secor.sb %>% 
  filter(transmitter != 'A69-1601-25465') %>% 
  merge(tag.data, all.x = T) %>%
  rbind(firsttagging.25465, secondtagging.25465) %>%
  tbl_df()

save(secor.sb, file = 'secor.sb.rda')
