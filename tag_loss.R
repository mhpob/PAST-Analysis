load('secor.sb.rda')

library(dplyr); library(lubridate)
max.date <- group_by(secor.sb, transmitter) %>%
  summarize(max = max(date.utc))
max14 <- filter(max.date, 
                transmitter %in% paste0('A69-1601-', seq(25434, 25533, 1))) %>% 
  mutate(interval = interval(ymd_hms('2014-04-01 00:00:00'), max))

max16 <- filter(max.date,
                transmitter %in% paste0('A69-1601-', seq(53850, 53899, 1))) %>% 
  mutate(interval = interval(ymd_hms('2014-05-13 00:00:00'), max))



surv14 <- data.frame(date = seq.Date(ymd('2014-04-01'), ymd('2016-12-31'),
                                       by = 'day'))
surv14$num <- sapply(surv14$date,
                     function(x) sum(x %within% max14$interval == T))
surv14$pct <- surv14$num/100

surv16 <- data.frame(date = seq.Date(ymd('2016-05-13'), ymd('2016-12-31'),
                                     by = 'day'))
surv16$num <- sapply(surv16$date,
                     function(x) sum(x %within% max16$interval == T))
surv16$pct <- surv16$num/50

library(ggplot2)
ggplot() + geom_line(data = surv14, aes(x = date, y = num)) +
  labs(x = 'Date', y = 'Percent 2014-tagged remaining', color = 'Array') +
  theme_bw()

ggplot() + geom_line(data = surv16, aes(x = date, y = pct*100)) +
  labs(x = 'Date', y = 'Percent 2016-tagged remaining', color = 'Array') +
  theme_bw()


