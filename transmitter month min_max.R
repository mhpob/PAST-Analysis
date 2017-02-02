library(TelemetryR); library(lubridate); library(dplyr)

load('secor.sb.rda')

# Months b/w 1st and last detections per fish (transmitter)
# 1) 1st and last dates per trans
# 2) Subtract dates

# 1) 
# Split into a list according to transmitter
# ?split
# class(secor.sb.$transmitter)
# [1] "character"


# 1b) Group w/in data frame

secor.sb <- group_by(secor.sb, transmitter)

secor.sb <- summarize(secor.sb,
                  low = min(date.local),
                  high = max(date.local))
# secor.sb["difference"] <- interval(secor.sb$low, secor.sb$high)
# secor.sb_grouped$diff <- interval(secor.sb$low, secor.sb$high)

secor.sb$high <- ymd_hms(secor.sb$high)
as.numeric(secor.sb$high)

min(secor.sb$high)
max(secor.sb$high)


secor.sb <- secor.sb$high(seq.Date(from = as.Date("2014-04-04 21:45:28 UTC"), to = as.Date("2017-01-03 21:51:16"), by = "month")




# hist


hist(secor.sb$high, breaks = 144, col="green")

#secor.sb$month <- months(secor.sb)#secor.sb$month <- month(secor.sb$high[1]:secor.sb$high[144])
