library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

# Data prep ----
# Select fish that lived long enough, flag those that didn't make it through
#   2015 season (mutate line)
valid.fish <- secor.sb %>% 
  filter(tag.date <= '2014-10-30') %>% 
  group_by(transmitter) %>%
  summarize(max = max(date.local)) %>% 
  filter(max >= '2015-03-31') %>% 
  mutate(yr.flag = ifelse(max <= '2016-03-31', 2014, 2015))

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

pct.coastal <- occ.data %>% 
  filter(coastal == T) %>% 
  distinct(transmitter, date.floor, .keep_all = T) %>%
  group_by(transmitter, age.adjust) %>% 
  summarize(pct.coastal = n()/12)

occ.data <- left_join(occ.data, pct.coastal) %>% 
  group_by(transmitter, year) %>% 
  mutate(coastal = ifelse(1 %in% coastal, 1, 0),
         pct.coastal = ifelse(is.na(pct.coastal), 0, pct.coastal)) %>%
  ungroup() %>% 
  distinct(tag.season, transmitter, year, coastal,
           pct.coastal, age.adjust, length)

#   Run coastal ~ age + random(fish) for age
#   Run years separately for length, shouldn't need random transmitter effect
rm(pct.coastal, secor.sb, valid.fish)
spring <- filter(occ.data, tag.season == 'Spring')
sp.2014 <- filter(spring, year == 2014)
sp.2015 <- filter(spring, year == 2015)

# Coastal incidence v age modeling ----
library(lme4)
glm_incidence <- glmer(coastal ~ age.adjust + (1 | transmitter),
                       family = 'binomial',
                       data = spring)

# Likelihood glmm
# library(ez)
# glm_incidence_ML <- ezMixed(data = spring,
#                            dv = .(coastal),
#                            fixed = .(age.adjust),
#                            random = .(transmitter),
#                            family = 'binomial')

# Coastal incidence v length modeling ----
glm_incidence14 <- glm(coastal ~ length,
                       data = sp.2014,
                       family = 'binomial')
glm_incidence15 <- glm(coastal ~ length,
                       data = sp.2015,
                       family = 'binomial')

# % Mmonths coastal v age ----
library(lme4)
glm_PctCoast <- glmer(pct.coastal ~ age.adjust + (1 | transmitter),
                      family = 'binomial',
                      data = spring)

# Likelihood glmm
# library(ez)
# glm_PctCoast_ML <- ezMixed(data = spring,
#                            dv = .(pct.coastal),
#                            fixed = .(age.adjust),
#                            random = .(transmitter),
#                            family = 'binomial')

# % Months coastal v length modeling ----
glm_PctCoast14 <- glm(pct.coastal ~ length,
                       data = sp.2014,
                       family = 'binomial')
glm_PctCoast15 <- glm(pct.coastal ~ length,
                       data = sp.2015,
                       family = 'binomial')