library(dplyr)

ages <- read.csv('p:/obrien/biotelemetry/past sb/ages.csv')
k <- ages %>%
  group_by(FishID) %>% 
  summarize(ages = mean(Age_scale)) %>% 
  mutate(year = ifelse(grepl('2014', FishID), 2014, 2016)) %>% 
  group_by(year) %>% 
  summarize(min = min(ages),
            mean = mean(ages),
            median = median(ages),
            max = max(ages))
