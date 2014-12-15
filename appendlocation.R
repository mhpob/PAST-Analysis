# Some data from MD DNR comes in with location missing
dnr <- data.frame(receiver = c('VR2W-106474', 'VR2W-102036', 'VR2W-106473',
                               'VR2W-106478'),
                station1 = c('Kent Island A', 'Kent Island B', 'Kent Island C',
                             'Kent Island D'),
                  lat1 = c(38.9953333, 38.9913167, 38.9841500,
                           38.9799167),
                  long1 = c(-76.3995333, -76.3902333, -76.3727833,
                            -76.3539667),
                  stringsAsFactors = F)
  
detects <- merge(detects, dnr, all.x = T)
detects$station <- ifelse(is.na(detects$station), detects$station1,
                          detects$station)
detects$lat <- ifelse(is.na(detects$lat), detects$lat1, detects$lat)
detects$long <- ifelse(is.na(detects$long), detects$long1, detects$long)
detects <- detects[,-c(9:11)]
detects <- detects[, c(2, 1, 3:8)]