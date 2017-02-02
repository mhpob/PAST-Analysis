library(readxl)
potomac <- read_excel("P:/Wiernicki/Potomac River WT.xlsx")
total_potomac <- lapply(excel_sheets("P:/Wiernicki/Potomac River WT.xlsx"), read_excel, path = "P:/Wiernicki/Potomac River WT.xlsx")


names(total_potomac) <- excel_sheets("P:/Wiernicki/Potomac River Wt.xlsx")

names(total_potomac[[1]])
names(total_potomac[[2]])

total_potomac <- total_potomac[1:2]



total_potomac[[1]]$site <- names(total_potomac[1])
total_potomac[[2]]$site <- names(total_potomac[2])

total_potomac <- do.call(rbind.data.frame, total_potomac[1:2])

total_potomac <- total_potomac[-1,]

total_potomac[total_potomac == "99"] <- NA
total_potomac[total_potomac == "999"] <- NA
total_potomac[total_potomac == "9999"] <- NA

total_potomac$date <- paste(total_potomac$YY, total_potomac$MM, total_potomac$DD, total_potomac$hh, total_potomac$mm, sep = "-")

# total_potomac <- total_potomac[,-1]
# total_potomac <- total_potomac[,-1]
# total_potomac <- total_potomac[,-1]
# total_potomac <- total_potomac[,-1]
# total_potomac <- total_potomac[,-1]
total_potomac <- total_potomac[,-seq(1,5,1)]


test <- total_potomac
test$date <- lubridate::ymd_hm(total_potomac$date)
test[is.na(test$date),]

# total_potomac$WDIR <- as.numeric(total_potomac$WDIR)
# total_potomac$WSPD <- as.numeric(total_potomac$WSPD)
# total_potomac$GST <- as.numeric(total_potomac$GST)
# total_potomac$WVHT <- as.numeric(total_potomac$WVHT)
# total_potomac$DPD <- as.numeric(total_potomac$DPD)
# total_potomac$APD <- as.numeric(total_potomac$APD)
# total_potomac$MWD <- as.numeric(total_potomac$MWD)
# total_potomac$PRES <- as.numeric(total_potomac$PRES)
# total_potomac$ATMP <- as.numeric(total_potomac$ATMP)
# total_potomac$WTMP <- as.numeric(total_potomac$WTMP)
# total_potomac$DEWP <- as.numeric(total_potomac$DEWP)
# total_potomac$VIS <- as.numeric(total_potomac$VIS)
# total_potomac$TIDE <- as.numeric(total_potomac$TIDE)


