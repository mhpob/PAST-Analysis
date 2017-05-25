load('secor.sb.rda')

library(dplyr); library(lubridate)
max.date <- group_by(secor.sb, transmitter) %>%
  summarize(max = max(date.utc))
max14s <- filter(max.date, 
                transmitter %in% paste0('A69-1601-', seq(25434, 25505, 1))) %>% 
  mutate(interval = interval(ymd_hms('2014-04-11 00:00:00'), max))

max14f <- filter(max.date, 
                 transmitter %in% paste0('A69-1601-', seq(25506, 25533, 1))) %>% 
  mutate(interval = interval(ymd_hms('2014-10-30 00:00:00'), max))

max16 <- filter(max.date,
                transmitter %in% paste0('A69-1601-', seq(53850, 53899, 1))) %>% 
  mutate(interval = interval(ymd_hms('2014-05-13 00:00:00'), max))



surv14s <- data.frame(date = seq.Date(ymd('2014-04-11'), ymd('2016-12-31'),
                                       by = 'day'),
                      tagging = 'Spring')
surv14s$num <- sapply(surv14s$date,
                     function(x) sum(x %within% max14s$interval == T))
surv14s$pct <- surv14s$num/72

surv14f <- data.frame(date = seq.Date(ymd('2014-10-30'), ymd('2016-12-31'),
                                      by = 'day'),
                      tagging = 'Fall')
surv14f$num <- sapply(surv14f$date,
                      function(x) sum(x %within% max14f$interval == T))
surv14f$pct <- surv14f$num/28

surv14 <- rbind(surv14s, surv14f)


surv16 <- data.frame(date = seq.Date(ymd('2016-05-13'), ymd('2016-12-31'),
                                     by = 'day'))
surv16$num <- sapply(surv16$date,
                     function(x) sum(x %within% max16$interval == T))
surv16$pct <- surv16$num/50

library(ggplot2)
ggplot() + geom_line(data = surv14, aes(x = date, y = pct*100, color = tagging)) +
  labs(x = 'Date', y = 'Percent 2014-tagged detected', color = 'Event') +
  theme_bw()

ggplot() + geom_line(data = surv16, aes(x = date, y = pct*100)) +
  labs(x = 'Date', y = 'Percent 2016-tagged detected') +
  theme_bw()


