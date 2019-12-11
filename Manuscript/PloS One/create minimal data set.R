load('secor.sb.rda')

secor.sb <- secor.sb %>% 
  filter(date.local <= '2018-12-31',
         grepl('-25', transmitter)) %>% 
  mutate(date.floor = lubridate::floor_date(date.local, 'day')) 

detections <- secor.sb %>% 
  distinct(transmitter, date.floor, station, .keep_all = T) %>% 
  select(transmitter, date.floor, lat, long, array) %>% 
  rename(date = 'date.floor')

write.csv(detections, 'manuscript/plos one/secor_detections.csv', row.names = F)



tagging_data <- secor.sb %>% 
  select(transmitter, tag.date:age) %>% 
  distinct(transmitter, tag.date, .keep_all = T) %>% 
  rename(length.mm = 'length', weight.kg = 'weight', age.yrs = 'age')

write.csv(metadata, 'manuscript/plos one/secor_tagging_data.csv', row.names = F)
