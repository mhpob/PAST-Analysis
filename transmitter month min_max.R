library(TelemetryR); library(lubridate); library(dplyr)

detects <- vemsort('p:/obrien/biotelemetry/detections')

secor.sb <- detects %>% 
  filter(trans.num >= 25434 & trans.num <= 25533 |
         trans.num >= 53850 & trans.num <= 53899) %>%
  select(-one_of('trans.name', 'trans.serial', 'sensor.value',
                 'sensor.unit')) %>% 
  data.frame()


detects <- vemsort('p:/obrien/biotelemetry/detections')

seq1 <- seq(25434, 25533, 1)
seq2 <- seq(53850, 53899, 1)

secor.sb <- detects$transmitter %>%
  paste("A69-1601", seq1:seq, sep="-")%>%
  select(-one_of('trans.name', 'trans.serial','sensor.value', 'sensor_unit
  


data.frame()
 






# secor.sb <- detects[(detects$trans.num >= 25434 & detects$trans.num <= 25533) |
#                       (detects$trans.num >= 53850 & detects$trans.num <= 53899),]
# secor.sb <- data.frame(secor.sb)[, 4:7]

# Months b/w 1st and last detections per fish (transmitter)
# 1) 1st and last dates per trans
# 2) Subtract dates

# 1) 
# Split into a list according to transmitter
?split
class(secor.sb$transmitter)
[1] "character"

secor.sb$factor_transmitter <- factor(secor.sb$transmitter)

# find min/max
class(secor.sb$date.local)
?month
?year

secor.sb_date_min<-lapply(secor.sb$date.utc, min)
secor.sb_date_max<-lapply(secor.sb$date.utc, max)

secor.sb_date_max
secor.sb_date_min


# 2) substract lists?

#secor.sb_date_mid<-mapply('-', secor.sb_date_max, secor.sb_date_min, SIMPLIFY=FALSE)
#secor.sb_date_mid<-diff(secor.sb_date_max,secor.sb_date_min)
#secor.sb_date_mid<-lapply(diff(secor.sb_date_max, secor.sb_date_min))

#min and max are presented as numbers of seconds or still "dates"? how to convert?



#v.secor.sb_date_max <- c(secor.sb_date_max)
#v.secor.sb_date_min <- c(secor.sb_date_min)

#v.secor.sb_date_mid <- diff(v.secor.sb_date_max, v.secor.sb_date_min)


# 1b) Group w/in data frame

secor.sb <- group_by(secor.sb, transmitter)

test <- summarize(secor.sb,
                  low = min(date.local),
                  high = max(date.local))


test$diff <- interval(test$low, test$high)

ftest<-test$diff/dmonth

ftest

# hist


hist(ftest, seq(0,148,1),col="green",main="")



