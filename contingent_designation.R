library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

dat <- secor.sb %>% 
  filter(!grepl('-53', secor.sb$transmitter)) %>% 
  mutate(yr.adjust = ifelse(date.local <= '2015-03-21', 2014,
                            ifelse(date.local > '2015-03-21' &
                                     date.local <= '2016-03-21', 2015,
                                   ifelse(date.local > '2016-03-21' &
                                            date.local <= '2017-03-21', 2016,
                                          2017))))
## Coastal Designation per year----
max.date <- dat %>% 
  group_by(transmitter) %>%
  summarize(max = max(date.local),
            max = year(max))

coastal <- dat %>%
  mutate(coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                       'Delaware', 'Hudson', 'Long Island',
                                       'Mass', 'New Jersey'), T, F)) %>% 
  group_by(transmitter, yr.adjust) %>% 
  filter(T %in% coastal) %>% 
  distinct(transmitter) %>% # years coastal
  full_join(max.date) %>% 
  mutate(coastal = ifelse(is.na(yr.adjust), 0, 1)) %>% #Mark non-coastal fish
  tidyr::spread(yr.adjust, coastal)

# Final labels. NA = removed (false negative), 0 = non coastal (negative), 
#   1 = coastal (positive)
for(i in 3:6){
  coastal[i] <- ifelse(is.na(coastal[i]) &
                         coastal$max >= as.numeric(names(coastal)[i]), 0,
        ifelse(coastal[, i] == 1, 1,
                   NA))
}

coastal <- coastal[, !names(coastal) == '<NA>']
names(coastal)[3:6] <- paste0('Coastal', names(coastal)[3:6])

# Spawning area
spawn <- read.csv('p:/obrien/biotelemetry/past sb/spawn designations.csv')
all <- left_join(coastal, spawn)

# Tagging, age, and sample location data
tag <- read.csv('p:/obrien/biotelemetry/past sb/taggingdata.csv')
all <- left_join(all, tag, by = c('transmitter' = 'Transmitter'))

all <- all[, names(all) %in% c('transmitter',
                               grep('Coast|Spawn', names(all), value = T),
                               'Tag.Date', 'Length..TL..mm.', 'Weight..kg.',
                               'Sex', 'Age.Scale', 'Scale.Loc..Age.',
                               'Scale.Loc..Genetics.')]
names(all) <- c('Transmitter', grep('Coast|Spawn', names(all), value = T),
                'Tag.Date', 'Length_mm', 'Weight_kg', 'Sex', 'Scale_age',
                'Scale.location_age', 'Scale.location_genetics')
all <- all[, c('Transmitter', 'Tag.Date', 'Length_mm', 'Weight_kg', 'Sex',
               'Scale_age', 'Scale.location_age', 'Scale.location_genetics',
               grep('Coast|Spawn', names(all), value = T))]

write.csv(all, 'PAST_geneticinfo.csv', row.names = F)
