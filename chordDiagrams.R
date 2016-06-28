load('secor.sb.rda')

secor.sb$season <- ifelse(lubridate::month(secor.sb$date.local) %in% c(3, 4, 5),
                          'Spring',
                   ifelse(lubridate::month(secor.sb$date.local) %in% c(6, 7, 8),
                          'Summer',
                   ifelse(lubridate::month(secor.sb$date.local) %in% c(9, 10, 11),
                          'Fall', 'Winter')))

secor.sb$seas_yr <- ifelse(grepl('Winter', secor.sb$season) &
                             lubridate::month(secor.sb$date.local) == 12,
                           paste(secor.sb$season,
                                 lubridate::year(secor.sb$date.local) + 1,
                                 sep = "_"),
                           paste(secor.sb$season,
                                 lubridate::year(secor.sb$date.local),
                                 sep = "_"))

secor.sb$array <- ifelse(grepl('Poto', secor.sb$array), 'Potomac',
                  ifelse(grepl('MD Bay', secor.sb$array), 'MD Bay',
                         secor.sb$array))

seasons <- split(secor.sb, secor.sb$seas_yr)


# First and last location for each fish in each season
test <- seasons[[1]]
library(dplyr)
test <- test %>% arrange(trans.num, date.local)
test <- split(test, test$trans.num)
t2 <- lapply(test, function(x){x[c(1,dim(x)[1]),]})
t2 <- lapply(t2, function(x){t(x[,'array'])})
t2 <- do.call(rbind.data.frame, t2)
names(t2) <- c('from', 'to')
t2 <- count(t2, from, to)
# Or: t2 <- xtabs(~from + to, t2)

library(circlize)
chordDiagram(t2, directional = 1, self.link = 1,
             order = c('Potomac', 'MD Bay', 'Patuxent', 'Rappahannock', 'James',
                       'Bay Mouth', 'MD Coast', 'DE Coast', 'New Jersey',
                       'Long Island', 'Mass'))
               # order = c('Mid Potomac', 'Lower Potomac', 'Lower MD Bay',
               #           'Patuxent', 'Mid MD Bay', 'Upper MD Bay', 'James',
               #           'Bay Mouth', 'MD Coast', 'DE Coast', 'New Jersey',
               #           'Long Island', 'Mass'))
