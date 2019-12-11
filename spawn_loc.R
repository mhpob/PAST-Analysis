library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

dat <- secor.sb %>% 
  filter(grepl('-25', secor.sb$transmitter),
         date.local >= '2015/03/01') %>% 
  mutate(yr.adjust = ifelse(month(date.local) %in% 1:2, year(date.local) - 1,
                            year(date.local)),
         doy = yday(date.local),
         wk = week(date.local)
  )

dat <- dat %>% 
  group_by(transmitter) %>% 
  summarize(maxyr = max(yr.adjust)) %>% 
  left_join(dat)

k <- split(dat, dat$yr.adjust)
kk <- lapply(k, function(x) split(x, x$transmitter))

g <- lapply(kk, function(x) lapply(x, function(y) filter(y, doy >= 74, doy <= 166)))
gg <- lapply(g, function(x) lapply(x, function(y) distinct(y, array, wk) %>% arrange(wk)))

gg[[1]]
