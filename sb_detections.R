library(TelemetryR); library(lubridate); library(dplyr)

# False detections as determined by VEMCO
false.pos <- c("A69-1303-15268", "A69-1303-21996", "A69-1303-55828",
               "A69-1601-13358", "A69-1601-14295", "A69-1601-18147",
               "A69-1601-20794", "A69-1601-21435", "A69-1601-25631",
               "A69-1601-27179", "A69-1601-31594", "A69-1601-37119",
               "A69-1601-41805", "A69-1601-43368", "A69-1601-43862",
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
  
secor.sb$array <- ifelse(arr('cbl'), 'CBL Pier',
            ifelse(arr('cedar'), 'Cedar Point',
            ifelse(arr('piney'), 'Piney Point',
            ifelse(arr('pot'), 'CBIBS',
            ifelse(arr('301'), 'Rt 301',
            ifelse(arr('kent'), 'Kent Island',
            ifelse(arr('chop'), 'Choptank',
            ifelse(arr('marsh'), 'Marshyhope',
            ifelse(arr('nan'), 'Nanticoke',
            ifelse(arr('poco'), 'Pocomoke',
            ifelse(arr('repo'), 'Reports',
            ifelse(arr('dmf') | arr('vine'), 'Mass',
            ifelse(secor.sb$station %in% c('Alexandria', 'Dogue Creek',
                    'Hains Point', 'Mattawoman', 'National Harbor',
                    'Piscataway', 'Pomonkey', 'Radar Tower',
                    'Roosevelt Br.', 'S. Capitol Br.', 'S. Craney Isl.'),
                         'DDOE',
            ifelse(secor.sb$station %in% c('Benedicts Bridge', 'Broomes',
                    'Jacks North', 'Jacks South', 'Magruders', 'Pepco',
                    'SERC Active'),
                         'SERC',
            ifelse(secor.sb$station %in% c('CC LS', 'LC2', 'NCD', 'NN 1ER FWS',
                    'NN 22 NOAA SP', 'NN DANGER FWS', 'Y wat'), 'Navy',
                    'Other')))))))))))))))

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
