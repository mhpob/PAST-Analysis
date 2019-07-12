library(TelemetryR); library(lubridate); library(dplyr)

# Import/Munging ----
cl <- parallel::makeCluster(parallel::detectCores() - 2)
detects <- vemsort('p:/obrien/biotelemetry/detections', clust = cl, prog_bar = T)
parallel::stopCluster(cl)

secor.sb <- detects %>% 
  filter(transmitter %in% paste0('A69-1601-', seq(25434, 25533, 1)) |
         transmitter %in% paste0('A69-1601-', seq(53850, 53899, 1))) %>%
  select(-one_of('trans.name', 'trans.serial', 'sensor.value',
                 'sensor.unit')) %>% 
  mutate(array = NA) %>% 
  data.frame()

array_greps <- list(
  'Lower Potomac' = 'piney|pot|look',
  'Mid Potomac' = '301|rad',
  'Patuxent' = 'cbl|jacks|pax|nott|jug',
  'Choptank' = 'chop',
  'Mid MD Bay' = 'kent|annap|dock|rho|thomas',
  'Lower MD Bay' = 'cedar|goose|tangier',
  'Upper MD Bay' = 'elk|pata',
  'C&D' = '&|back creek',
  'Nanticoke' = 'marshyhope|nanti',
  'Rappahannock' = 'rapp|sting|cr ',
  'York' = 'vims|^y',
  'James' = '^nn|(g|^r)\\d|bur_| poco|^hi|james|NH(1*)(8|0)',
  'MD Coast' = '([vat]|cs)-|inner|outer|middle|[iao][nms]\\d',
  'Delaware' = '# 2... ([DBN]|Ch)|# 3|philly|barge',
  'DE Coast' = '# 1|BOEM|gate',
  'New Jersey' = 'sandy|barnegat|shark river|egg|garden',
  'Hudson' = '^light|storm|nysta',
  'NYB' = 'stony',
  'Long Island' = 'fire|inlet \\d|jones|montauk|rockaway|shinnecock|
              |swg|thames',
  'Mass' = '(ca|cz|bb|bh|^er|ph|vs)\\d|dmf|vine|nera|plum|joppa|nau|
              |chat|mono|cove|elli| inl|orl|sci|well|ccc|sandwich|guard|ledge|
              |province|cutty|mano|barns|marshfield|hull|marble|dux|pam',
  'Bay Mouth' = '(^b|ts)\\d|CBB|LC|henry|cc ls|\\dch|^ch$',
  'VA Coast' = '(^cb|ri)($|\\d| )|^nc|^ra$|rao|scl|wea'
)

addons <- list(
  'Mid Potomac' = 'Report1',
  'Upper Potomac' = c('Alexandria', 'Dogue Creek', 'Hains Point', 'Mattawoman',
                      'Mallows Bay' ,'National Harbor', 'Piscataway',
                      'Pomonkey', 'Roosevelt Br.', 'S. Capitol Br.',
                      'S. Craney Isl.', 'Report2', 'Report8'),
  'Patuxent' = c('Benedicts Bridge', 'Broomes', 'Magruders',
                 'Pepco', 'SERC Active'),
  'Mid MD Bay' = c('Report3', 'Report7', 'Report9'),
  'Upper MD Bay' = 'Report5',
  'Elizabeth' = c('APM1', paste0('NH', 12:35)),
  'Long Island' = 'Report4'
  )

station_list <- lapply(array_greps,
                       grep, x = unique(secor.sb$station),
                       ignore.case = T, value = T)

for(i in seq(1:length(names(addons)))){
  station_list[[names(addons)[i]]] <-
    c(station_list[[names(addons)[i]]], addons[[i]])
}


# Array designation test
grep_check <- sapply(1:length(station_list),
                     function(n) intersect(station_list[[n]],
                                           unlist(station_list[-n])))
if(length(unlist(grep_check)) != 0){
  names(grep_check) <- names(station_list)
  print(unlist(grep_check))
  stop('Regex matches multiple stations!!')
}


# Designate arrays
for(i in seq(1:length(station_list))){
  secor.sb$array <- ifelse(secor.sb$station %in% station_list[[i]],
                            names(station_list)[i],
                            secor.sb$array)
}


# Missing array info test
if(dim(filter(secor.sb, is.na(array)))[1] > 1){
  stop('UNID array found!')
}


# Data joins and export ----

tag.data <- read.csv('p:/obrien/biotelemetry/PAST SB/taggingdata.csv',
                     stringsAsFactors = F)
tag.data$Date <- mdy(tag.data$Tag.Date, tz = 'America/New_York')
tag.data <- tag.data[, c('Tag.Date', 'Transmitter', 'Length..TL..mm.',
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
