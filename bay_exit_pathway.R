library(dplyr)
load('secor.sb.rda')

# Choose only those tagged in 2014
t2014 <- filter(secor.sb, grepl('-25', transmitter)) 

# 2014 ----
# Number heard in 2014
#   (all)
filter(t2014, date.local < '2015-01-01') %>% 
  distinct(transmitter) %>% 
  nrow

#   (spring-tagged only)
filter(t2014, date.local < '2015-01-01', grepl('[34]/', tag.date)) %>%
  distinct(transmitter) %>% 
  nrow


# Number that left bay in 2014
#   (all)
filter(t2014, date.local < '2015-01-01',
       array %in% c('Bay Mouth', 'C&D', 'DE Coast', 'Delaware', 'Hudson',
                    'Long Island', 'Mass', 'MD Coast', 'New Jersey', 'NYB',
                    'VA Coast')) %>% 
  distinct(transmitter) %>% 
  nrow

#   (spring-tagged only)
filter(t2014, date.local < '2015-01-01', grepl('[34]/', tag.date),
       array %in% c('Bay Mouth', 'C&D', 'DE Coast', 'Delaware', 'Hudson',
                    'Long Island', 'Mass', 'MD Coast', 'New Jersey', 'NYB',
                    'VA Coast')) %>% 
  distinct(transmitter) %>% 
  nrow


# Number that left through C&D
#   (all)
filter(t2014, date.local < '2014-06-01',
       array %in% c('C&D', 'Delaware')) %>% 
  distinct(transmitter) %>% 
  nrow

#   (spring-tagged only)
filter(t2014, date.local < '2014-06-01', grepl('[34]/', tag.date),
       array %in% c('C&D', 'Delaware')) %>% 
  distinct(transmitter) %>% 
  nrow


# Number that left through Mouth
#   (all)
filter(t2014, date.local < '2014-06-01',
       array %in% c('Bay Mouth', 'VA Coast', 'MD Coast')) %>% 
  distinct(transmitter) %>% 
  nrow

#   (spring-tagged only)
filter(t2014, date.local < '2014-06-01', grepl('[34]/', tag.date),
       array %in% c('Bay Mouth', 'VA Coast', 'MD Coast')) %>% 
  distinct(transmitter) %>% 
  nrow

# 2015 ----
# Number heard in 2015
filter(t2014, date.local < '2016-01-01', date.local >= '2015-01-01') %>% 
  distinct(transmitter) %>% 
  nrow


# Number that left bay in 2015
filter(t2014, date.local < '2016-01-01', date.local >= '2015-01-01',
       array %in% c('Bay Mouth', 'C&D', 'DE Coast', 'Delaware', 'Hudson',
                    'Long Island', 'Mass', 'MD Coast', 'New Jersey', 'NYB',
                    'VA Coast')) %>% 
  distinct(transmitter) %>% 
  nrow


# Number that left through C&D
filter(t2014, date.local < '2015-06-01', date.local >= '2015-01-01',
       array %in% c('C&D', 'Delaware')) %>% 
  distinct(transmitter) %>% 
  nrow


# Number that left through Mouth
filter(t2014, date.local < '2015-06-01', date.local >= '2015-01-01',
       array %in% c('Bay Mouth', 'VA Coast', 'MD Coast')) %>% 
  distinct(transmitter) %>% 
  nrow

# 2016 ----
# Number heard in 2016
filter(t2014, date.local < '2017-01-01', date.local >= '2016-01-01') %>% 
  distinct(transmitter) %>% 
  nrow


# Number that left bay in 2016
filter(t2014, date.local < '2017-01-01', date.local >= '2016-01-01',
       array %in% c('Bay Mouth', 'C&D', 'DE Coast', 'Delaware', 'Hudson',
                    'Long Island', 'Mass', 'MD Coast', 'New Jersey', 'NYB',
                    'VA Coast')) %>% 
  distinct(transmitter) %>% 
  nrow


# Number that left through C&D
filter(t2014, date.local < '2016-06-01', date.local >= '2016-01-01',
       array %in% c('C&D', 'Delaware')) %>% 
  distinct(transmitter) %>% 
  nrow


# Number that left through C&D
filter(t2014, date.local < '2016-06-01', date.local >= '2016-01-01',
       array %in% c('C&D', 'Delaware')) %>% 
  distinct(transmitter) %>% 
  nrow


# Number that left through Mouth
filter(t2014, date.local < '2016-06-01', date.local >= '2016-01-01',
       array %in% c('Bay Mouth', 'VA Coast', 'MD Coast')) %>% 
  distinct(transmitter) %>% 
  nrow


# 2017 ----
# Number heard in 2017
filter(t2014, date.local < '2018-01-01', date.local >= '2017-01-01') %>% 
  distinct(transmitter) %>% 
  nrow


# Number that left bay in 2017
filter(t2014, date.local < '2018-01-01', date.local >= '2017-01-01',
       array %in% c('Bay Mouth', 'C&D', 'DE Coast', 'Delaware', 'Hudson',
                    'Long Island', 'Mass', 'MD Coast', 'New Jersey', 'NYB',
                    'VA Coast')) %>% 
  distinct(transmitter) %>% 
  nrow


# Number that left through C&D
filter(t2014, date.local < '2017-06-01', date.local >= '2017-01-01',
       array %in% c('C&D', 'Delaware')) %>% 
  distinct(transmitter) %>% 
  nrow


# Number that left through Mouth
filter(t2014, date.local < '2017-06-01', date.local >= '2017-01-01',
       array %in% c('Bay Mouth', 'VA Coast', 'MD Coast')) %>% 
  distinct(transmitter) %>% 
  nrow
