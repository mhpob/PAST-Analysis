library(readxl)

WTMP <- function(x){
  data.list <- lapply(excel_sheets(x),
                      read_excel, path = x)
  d.l_length <- length(data.list)
  
  for(i in seq(1, d.l_length - 1)){
    data.list[[i]]$site <-
      data.frame(data.list[[d.l_length]])[i, 3]
  }
  
  data.list[[d.l_length]]$lat <- substr(data.frame(
    data.list[[d.l_length]])[, 'Station.Coordinates'],
    1, 6)
  data.list[[d.l_length]]$long <- substr(data.frame(
    data.list[[d.l_length]])[, 'Station.Coordinates'],
    10, 15)
  
  data.list <- lapply(data.list[1: d.l_length - 1], merge,
                      y = data.list[[d.l_length]][,
                                      c('Additional Notes','lat','long')],
                      by.x = 'site', by.y = 'Additional Notes')
  
  data <- do.call(rbind, data.list)
  
  data$date <- paste(data$YY, data$MM,
                     data$DD, data$hh,
                     data$mm, sep = "-")
  data$date <- lubridate::ymd_hm(data$date)
  
  data <- data[, c('site', 'lat', 'long', 'date', 'WTMP')]
  
  data[data$WTMP == 999, 'WTMP'] <- NA
  row.names(data) <- NULL
  
  data
}

potomac <- WTMP('P:/Wiernicki/Potomac River Wt.xlsx')
coastal <- WTMP('P:/Wiernicki/East Coast WT.xlsx')