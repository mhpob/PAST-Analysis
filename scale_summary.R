library(ggplot2); library(dplyr)

ages <- read.csv('p:/obrien/biotelemetry/past sb/ages.csv')
age.summary <- ages %>%
  group_by(FishID) %>% 
  summarize(ages = mean(Age_scale)) %>% 
  mutate(year = ifelse(grepl('2014', FishID), 2014, 2016)) %>% 
  group_by(year) %>% 
  summarize(min = min(ages),
            mean = mean(ages),
            median = median(ages),
            max = max(ages))


year.class <- ages %>%
  group_by(FishID) %>% 
  summarize(ages = mean(Age_scale)) %>% 
  filter(grepl('2014', FishID)) %>% 
  mutate(year.class = floor(2014 - ages))


ggplot() + geom_histogram(data = year.class, aes(year.class),
                          binwidth = 1) +
  scale_x_continuous(breaks = seq(2000, 2012, 2)) +
  labs(x = 'Year Class', y = 'Count') +
  theme_bw()
