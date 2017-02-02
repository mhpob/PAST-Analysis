library(TelemetryR); library(lubridate); library(dplyr)

load('secor.sb.rda')

# Group transmitters w/in data frame, then find first and last detections
secor.sb <- group_by(secor.sb, transmitter)

secor.sb <- summarize(secor.sb,
                  low = min(date.local),
                  high = max(date.local))

# Histogram
hist(secor.sb$high,
     breaks = seq(from = ymd_hms('2014-03-30 00:00:00'),
                  to = max(secor.sb$high)+months(1),
                  by = 'months'),
     col="green")

# Cumulative frequency. Note: no built-in function

