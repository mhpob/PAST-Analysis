load('secor.sb.rda')

secor.sb <- secor.sb %>% 
  filter(date.local <= '2018-12-31',
         grepl('-25', transmitter)) %>% 
  mutate(date.floor = lubridate::floor_date(date.local, 'day')) %>% 
  distinct(transmitter, date.floor, station, .keep_all = T) %>% 
  select(transmitter, date.floor, lat, long, array)

write.csv(secor.sb, 'manuscript/plos one/Diff_Mig_CB_SB.csv', row.names = F)
