library(TelemetryR); library(lubridate); library(dplyr)

load('secor.sb.rda')

# Group transmitters w/in data frame, then find first and last detections
secor.sb <- group_by(secor.sb, transmitter)

secor.sb <- summarize(secor.sb,
                  low = min(date.local),
                  high = max(date.local))

# Histogram
hist(secor.sb$high,
     breaks = seq(from = ymd_hms('2014-03-30 00:00:00',
                                 tz = 'America/New_York'),
                  to = max(secor.sb$high) + months(1),
                  by = 'months'),
     col="green")

# Cumulative frequency
#   Note: no built-in function, so I'm going to cheat and use a built-in
#   function from the ggplot2 package
library(ggplot2)
CF_plot <- ggplot() + stat_ecdf(data = secor.sb, aes(x = high)) +
  scale_x_datetime(date_breaks = '3 months')
CF_plot

#   Pull data from the plot. Dates are returned in Unix (seconds since
#   Jan 1, 1970), so we need to convert back to a usable time.
CF_data <- data.frame(ggplot_build(CF_plot)$data)
CF_data$date.local <- as_datetime(CF_data$x, tz = 'America/New_York')

#   Find date where 90% of tags stopped transmitting
min(CF_data[CF_data$y >= 0.9, 'date.local'])

