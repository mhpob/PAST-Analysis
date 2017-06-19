load('secor.sb.rda')
library(TelemetryR); library(dplyr); library(lubridate)

surv14s <- filter(secor.sb,
                  transmitter %in% paste0('A69-1601-', seq(25434, 25505, 1))) %>%
  trans_loss(., dates = 'date.local', group = 'transmitter',
             stdate = ymd_hms('2014-04-11 00:00:00'),
             enddate = ymd_hms('2016-12-31 11:59:59')) %>% 
  mutate(tagging = 'Spring',
         pct = remaining / 72)


surv14f <- filter(secor.sb,
                  transmitter %in% paste0('A69-1601-', seq(25506, 25533, 1))) %>%
  trans_loss(., dates = 'date.local', group = 'transmitter',
             stdate = ymd_hms('2014-10-30 00:00:00'),
             enddate = ymd_hms('2016-12-31 11:59:59')) %>% 
  mutate(tagging = 'Fall',
         pct = remaining / 28)

surv14 <- rbind(surv14s, surv14f)


surv16 <- filter(secor.sb,
                transmitter %in% paste0('A69-1601-', seq(53850, 53899, 1))) %>%
  trans_loss(., dates = 'date.local', group = 'transmitter',
             stdate = ymd_hms('2016-05-05 00:00:00'),
             enddate = ymd_hms('2016-12-31 11:59:59')) %>% 
  mutate(pct = remaining / 50)


library(ggplot2)
ggplot() + geom_line(data = surv14, aes(x = date, y = pct * 100,
                                        color = tagging)) +
  labs(x = 'Date', y = 'Percent 2014-tagged detected', color = 'Event') +
  theme_bw()

ggplot() + geom_line(data = surv16, aes(x = date, y = pct * 100)) +
  labs(x = 'Date', y = 'Percent 2016-tagged detected') +
  lims(y = c(0, 100)) +
  theme_bw()
