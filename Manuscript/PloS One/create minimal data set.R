library(dplyr)
load('secor.sb.rda')

secor.sb <- secor.sb %>% 
  filter(grepl('-25', transmitter)) %>% 
  mutate(date = case_when(date.local >= '2019-01-01 00:00:00' ~
                            # Add dummy date if fish was heard after 2018
                            lubridate::ymd_hms('2019-01-01 00:00:01', tz = 'America/New_York'),
                      T ~ date.local), 
         date = lubridate::floor_date(date, 'day'),
         transmitter = case_when(transmitter == 'A69-1601-25465' &
                                   date.utc <= '2014-06-15' ~ 'A69-1601-25465a',
                                 transmitter == 'A69-1601-25465' &
                                   date.utc > '2014-06-15' ~ 'A69-1601-25465b',
                                 T ~ transmitter))

detections <- secor.sb %>% 
  distinct(transmitter, date, station, .keep_all = T) %>% 
  select(transmitter, date, lat, long, array)

write.csv(detections, 'manuscript/plos one/secor_detections.csv', row.names = F)



tagging_data <- secor.sb %>% 
  select(transmitter, tag.date:age) %>% 
  distinct(transmitter, tag.date, .keep_all = T) %>% 
  rename(length.mm = 'length', weight.kg = 'weight', age.yrs = 'age')

write.csv(tagging_data, 'manuscript/plos one/secor_tagging_data.csv', row.names = F)
