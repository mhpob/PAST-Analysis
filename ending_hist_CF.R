library(TelemetryR); library(lubridate); library(dplyr)

load('secor.sb.rda')

# Group transmitters by cohort tagged, then find first and last detections
spring.tag <- filter(secor.sb, tag.date < '2014-10-30') %>% 
  group_by(transmitter)
fall.tag <- filter(secor.sb, tag.date == '2014-10-30') %>% 
  group_by(transmitter)

hi_lo <- function(data){
  summarize(data,
            low = min(date.local),
            high = max(date.local))
}

# Histogram
final_detect_hist <- function(data){
  new.data <- hi_lo(data)
  
  hist(new.data$high,
     breaks = seq(from = min(new.data$high),
                  to = max(new.data$high) + months(1),
                  by = 'months'),
     col="green",
     main = deparse(substitute(data)),
     xlab = 'Date')
}

final_detect_hist(spring.tag)
final_detect_hist(fall.tag)

# Cumulative frequency
library(ggplot2)
final_detect_cf <- function(data, pct.stop){
  new.data <- hi_lo(data)
  
  #   Note: no built-in function, so I'm going to cheat and use a built-in
  #   function from the ggplot2 package
  CF_plot <- ggplot() + stat_ecdf(data = new.data, aes(x = high)) +
    scale_x_datetime(date_breaks = '3 months')
  print(CF_plot)
  
  #   Pull data from the plot. Dates are returned in Unix (seconds since
  #   Jan 1, 1970), so we need to convert back to a usable time.
  CF_data <- data.frame(ggplot_build(CF_plot)$data)
  CF_data$date.local <- as_datetime(CF_data$x, tz = 'America/New_York')
  
  #   Find date where X% of tags stopped transmitting
  stopped <- min(CF_data[CF_data$date.local >= pct.stop, 'date.local'])
  cat((as.numeric(pct.stop) * 100), '% of last detections occurred before',
  as.character(stopped))
}

final_detect_cf(spring.tag, 0.9)
final_detect_cf(fall.tag, 0.75)
