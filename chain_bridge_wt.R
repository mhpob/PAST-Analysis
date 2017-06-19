library(waterData)

cb_temp <- importDVs("01646500", code = "00010", stat = "00003",
                     sdate = "2014-01-01", edate = "2016-12-31")
cb_temp <- aggregate(val ~ dates, cb_temp, FUN = mean)
cb_temp$var <- 'wtemp'
# plot(cb_temp$dates, cb_temp$val)


cb_disch <- importDVs("01646500", code = "00060", stat = "00003",
                     sdate = "2014-01-01", edate = "2016-12-31")
cb_disch$val <- cb_disch$val * 0.02831685
cb_disch$var <- 'discharge'
# plot(cb_disch$dates, cb_disch$val)

chainbr <- merge(cb_temp, cb_disch, all = T)[, 1:3]
