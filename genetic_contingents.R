library(lubridate); library(dplyr)
load('secor.sb.rda')


secor.sb <- secor.sb %>% 
  filter(transmitter %in% paste0('A69-1601-', seq(25434, 25533, 1)))

nan <- filter(secor.sb, array == 'Nanticoke')

unique(filter(secor.sb, date.local >= '2015-05-31')$transmitter)

# james <- mutate(james, spawn = ifelse(grepl('^G[147]|^R[145689]|HI|Ft|James', station), T, F) )
# pax <- mutate(pax, spawn = ifelse(grepl('Nott|Magr|Pep|#[(2A)|3]|Benedicts|SERC', station), T, F) )     

# Select fish that lived long enough
valid.fish <- secor.sb %>% 
  group_by(transmitter) %>%
  summarize(max = max(date.local)) %>% 
  filter(max >= '2016-03-31')

occ.data <- secor.sb %>% 
  left_join(valid.fish) %>% 
  # Use flag to remove observations if fish didn't make it through year
  mutate(yr.flag = ifelse(yr.flag == 2014 & date.local >= '2015-04-01',
                          NA, yr.flag)) %>% 
  filter(!is.na(yr.flag),
         date.local < '2016-04-01') %>% 
  mutate(tag.season = ifelse(tag.date == '2014-10-30', 'Fall', 'Spring'),
         date.floor = floor_date(date.local, unit = 'month'),
         year = ifelse(date.local < '2015-04-01', 2014, 2015),
         coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                       'Hudson', 'Long Island', 'Mass', 'New Jersey'),
                          1, 0),
         age.adjust = ifelse(date.local >= '2015-04-01',
                             age + 1, age))

library(TelemetryR)
sb.list <- split(secor.sb, secor.sb$transmitter)

library(parallel)
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, library(TelemetryR))

sb.list <- parLapply(cl, sb.list, track, dates = 'date.local', ids = 'array')

stopCluster(cl)

for(i in 1:length(sb.list)){
  sb.list[[i]]$transmitter <- names(sb.list)[i]
}
sb.tracks <- do.call(rbind.data.frame, sb.list)

# # Select fish that lived long enough
# valid.fish <- secor.sb %>% 
#   group_by(transmitter) %>%
#   summarize(max = max(date.local)) %>% 
#   filter(max >= '2016-03-31')
# 
# occ.data <- secor.sb %>% 
#   left_join(valid.fish) %>% 
#   # Use flag to remove observations if fish didn't make it through year
#   mutate(yr.flag = ifelse(yr.flag == 2014 & date.local >= '2015-04-01',
#                           NA, yr.flag)) %>% 
#   filter(!is.na(yr.flag),
#          date.local < '2016-04-01') %>% 
#   mutate(tag.season = ifelse(tag.date == '2014-10-30', 'Fall', 'Spring'),
#          date.floor = floor_date(date.local, unit = 'month'),
#          year = ifelse(date.local < '2015-04-01', 2014, 2015),
#          coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
#                                        'Hudson', 'Long Island', 'Mass', 'New Jersey'),
#                           1, 0),
#          age.adjust = ifelse(date.local >= '2015-04-01',
#                              age + 1, age))

# Detections in Apr and May
apr_may <- secor.sb %>% 
  filter(year(date.local) %in% 2015:2017,
         month(date.local) %in% 4:5)

not_heard <- filter(secor.sb, 
                    transmitter %in%
                      setdiff(levels(factor(secor.sb$transmitter)),
                              levels(factor(apr_may$transmitter)))) %>% 
  mutate(apr_may = 'notheard')


apr_may <- split(apr_may, apr_may$transmitter)
for(i in seq(1, length(apr_may), 1)){
  apr_may[[i]]$apr_may <- ifelse(T %in% grepl('Potomac', unique(apr_may[[i]]$array)),
                       'AllPot', 'NoPot')
  apr_may[[i]]$apr_may <- ifelse(unique(apr_may[[i]]$apr_may) == 'AllPot' &
                         F %in% grepl('Potomac', unique(apr_may[[i]]$array)),
                       'MixPot',
                       apr_may[[i]]$apr_may)
}

apr_may <- do.call(rbind, apr_may)
apr_may <- rbind(apr_may, not_heard)

apr_may <- apr_may %>% 
  distinct(transmitter, apr_may) %>% 
  arrange(transmitter)

apr_may %>% group_by(apr_may) %>% summarize(n())
