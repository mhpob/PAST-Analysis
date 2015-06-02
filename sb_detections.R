library(TelemetryR); library(lubridate); library(dplyr)

# False detections as determined by VEMCO
false.pos <- c("A69-1601-21435", "A69-1601-37119", "A69-1601-41805",
               "A69-1601-43030", "A69-1601-43368", "A69-1601-43862",
               "A69-1601-60533", "A69-1601-64288", "A69-1601-9185",
               "A69-1602-22686", "A69-1602-23019", "A69-1602-46762",
               "A69-1602-54302", "A69-1602-64173", "A69-1602-64407",
               "A69-9001-26563", "A69-9001-65126")
detects <- vemsort('p:/obrien/biotelemetry/detections', false.pos)

secor.sb <- detects %>% 
  filter(trans.num >= 25434 & trans.num <= 25533) %>%
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
      ifelse(arr('cbl|jacks|pax|nott') | secor.sb$station %in% c('Benedicts Bridge',
                  'Broomes', 'Magruders', 'Pepco', 'SERC Active'), 'Patuxent',
      ifelse(arr('chop'), 'Choptank',
      ifelse(arr('kent|annap') | secor.sb$station %in% c('Report3', 'Report7',
                                    'Report9'), 'Mid MD Bay',
      ifelse(arr('cedar|goose'), 'Lower MD Bay',
      ifelse(arr('elk|pata') | secor.sb$station == 'Report5', 'Upper MD Bay',
      ifelse(arr('&| 32'), 'C&D',
      ifelse(arr('rapp'), 'Rappahannock',
      ifelse(arr('vims|^y'), 'York',
      ifelse(arr('^nn') |
               secor.sb$station %in% c('NH8', 'NH10'), 'James',
      ifelse(secor.sb$station %in% c('APM1',
                                     paste0('NH', 12:35)), 'Elizabeth',
      ifelse(arr('v-|t-|a-'), 'MD Coast',
      ifelse(arr('# 2|# 3'), 'Delaware',
      ifelse(arr('# 1'), 'DE Coast',
      ifelse(arr('sandy|barnegat|shark'), 'New Jersey',
      ifelse(arr('fire|inlet \\d|jones|montauk|rockaway|shinnecock|swg') |
               secor.sb$station == 'Report4', 'Long Island',
      ifelse(arr('dmf|vine|cz|ph|nera|plum|joppa|^er|ca\\d|nau|chat|mono|cove'),
             'Mass',
      ifelse(arr('B1|CBB|LC|ts\\d|\\dch') |
               secor.sb$station %in% c('CC LS', 'CH', 'NCD'), 'Bay Mouth',
             'Other'))))))))))))))))))))

tag.data <- read.csv('p:/obrien/biotelemetry/striped bass/taggingdata.csv',
                     stringsAsFactors = F)
tag.data$Date <- mdy(tag.data$Date, tz = 'America/New_York')
tag.data <- tag.data[, c(1, 2, 5, 7, 8)]
names(tag.data) <- c('tag.date', 'trans.num', 'length', 'weight', 'sex')

# we reused tag A69-1601-25465 on 2014-10-30. Need to split tagging data to
# reflect this.
firsttagging.25465 <- secor.sb %>% 
  filter(trans.num == 25465, date.utc <= '2014-06-15') %>%
  merge(tag.data[32,], all.x = T)

secondtagging.25465 <- secor.sb %>% 
  filter(trans.num == 25465, date.utc >= '2014-10-29') %>%
  merge(tag.data[101,], all.x = T)

secor.sb <- secor.sb %>% 
  filter(trans.num != 25465) %>% 
  merge(tag.data, all.x = T) %>%
  rbind(firsttagging.25465, secondtagging.25465) %>%
  tbl_df()

save(secor.sb, file = 'secor.sb.rda')
