library(TelemetryR); library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

above301 <- secor.sb %>% 
  filter(year(tag.date) == 2014,
         month(date.local) %in% c(3, 4, 5)) %>% 
  mutate(above = ifelse(array %in% c('Mid Potomac', 'Upper Potomac'),
                        1, 0))

split.detections <- split(above301, above301$transmitter)
split.detections  <- lapply(split.detections, track,
                            dates = 'date.utc', ids = 'above')

valid <- lapply(split.detections, function(x) dim(x)[1])
valid <- split.detections[valid > 1]
for(i in seq(1, length(valid)[1])){
  valid[[i]]$transmitter <- names(valid)[i]
}

valid <- do.call(rbind, c(valid, make.row.names = F))
valid <- secor.sb %>% 
  distinct(transmitter, sex) %>% 
  right_join(valid)

time.above <- valid %>%
  mutate(year = year(date.utc),
         sex = ifelse(sex == '', 'Unknown', sex)) %>% 
  group_by(transmitter, year, sex) %>% 
  summarize(time.above = as.numeric(max(date.utc) - min(date.utc))) %>% 
  filter(time.above > 0)

ggplot() + geom_histogram(data = time.above, aes(time.above, fill = sex),
                          binwidth = 2) +
  facet_wrap(~year, ncol = 1) +
  labs(x = 'Days Above Rt 301', y = 'Count') +
  theme_bw()
