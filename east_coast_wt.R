library(readxl)
east_coast <- read_excel("P:/Wiernicki/East Coast WT.xlsx")

total_east_coast <- lapply(excel_sheets("P:/Wiernicki/East Coast WT.xlsx"), read_excel, path = "P:/Wiernicki/East Coast WT.xlsx")

test <- do.call(rbind.data.frame, total_east_coast)
?do.call

# Error in match.names(clabs, names(xi)) : 
#   names do not match previous names

names(east_coast)
names(total_east_coast[[1]])
total_east_coast[[1]][2,7]

names(total_east_coast[[1]])
names(total_east_coast[[2]])
lapply(total_east_coast, names)


names(total_east_coast) <- excel_sheets("P:/Wiernicki/East Coast WT.xlsx")
total_east_coast <- total_east_coast[1:8]

# 1) Assign name of the list element to a new column in the data frame in that part of the list
### list[[1]]$site <- names(list)[1]

# for(i in seq(1:9)){
# list[[i]]$site <- names(list)[i]
# }

total_east_coast[[1]]$site <- names(total_east_coast[1])
total_east_coast[[2]]$site <- names(total_east_coast[2])
total_east_coast[[3]]$site <- names(total_east_coast[3])
total_east_coast[[4]]$site <- names(total_east_coast[4])
total_east_coast[[5]]$site <- names(total_east_coast[5])
total_east_coast[[6]]$site <- names(total_east_coast[6])
total_east_coast[[7]]$site <- names(total_east_coast[7])
total_east_coast[[8]]$site <- names(total_east_coast[8])
#total_east_coast[[9]]$site <- names(total_east_coast[9])


# 2) rbind (do.call)

total_east_coast <- do.call(rbind.data.frame,total_east_coast[1:8])


#library(dplyr)
#total_east_coast <- bind_rows(total_east_coast[1:8],id=NULL)



# 3) take care of first row

total_east_coast <- total_east_coast[-1,]

# 4) tell R that 999 is NA
        # is.na?
        # maybe should have done during input?
        # just google it

total_east_coast[total_east_coast == "99"] <- NA
total_east_coast[total_east_coast == "999"] <- NA
total_east_coast[total_east_coast == "9999"] <- NA


# 5) Get dates right: paste() columns into new, ?lubridate::ymd_hm



total_east_coast$Date <- paste(total_east_coast$YY, total_east_coast$MM, total_east_coast$DD, total_east_coast$hh, total_east_coast$mm, sep="-")

total_east_coast <- total_east_coast[,-1]
total_east_coast <- total_east_coast[,-1]
total_east_coast <- total_east_coast[,-1]
total_east_coast <- total_east_coast[,-1]
total_east_coast <- total_east_coast[,-1]



#total_east_coast$Date <- as.data.frame.POSIXct(total_east_coast$Date)
#total_east_coast$Date[total_east_coast$Date == "0000"] <- NA

lubridate::ymd_hm(total_east_coast$Date)

head(total_east_coast$Date)
tail(total_east_coast$Date)



is.na(total_east_coast$Date)

total_east_coast[is.na(total_east_coast$Date),]


# 6) sort out classes of other columns: as.WHATEVERTHECLASSSHOULDBE
###   data$numeric <- as.numeric(data$numeric)

typeof(total_east_coast$WDIR)
typeof(total_east_coast$WSPD)
typeof(total_east_coast$GST)
typeof(total_east_coast$WVHT)
typeof(total_east_coast$DPD)
typeof(total_east_coast$APD)
typeof(total_east_coast$MWD)
typeof(total_east_coast$PRES)
typeof(total_east_coast$ATMP)
typeof(total_east_coast$WTMP)
typeof(total_east_coast$DEWP)
typeof(total_east_coast$VIS)
typeof(total_east_coast$TIDE)
typeof(total_east_coast$site)
typeof(total_east_coast$Date)

class(total_east_coast$Date)

# total_east_coast$WDIR <- as.numeric(total_east_coast$WDIR)
# total_east_coast$WSPD <- as.numeric(total_east_coast$WSPD)
# total_east_coast$GST <- as.numeric(total_east_coast$GST)
# total_east_coast$WVHT <- as.numeric(total_east_coast$WVHT)
# total_east_coast$DPD <- as.numeric(total_east_coast$DPD)
# total_east_coast$APD <- as.numeric(total_east_coast$APD)
# total_east_coast$MWD <- as.numeric(total_east_coast$MWD)
# total_east_coast$PRES <- as.numeric(total_east_coast$PRES)
# total_east_coast$ATMP <-as.numeric(total_east_coast$ATMP)
# total_east_coast$WTMP <- as.numeric(total_east_coast$WTMP)
# total_east_coast$DEWP <- as.numeric(total_east_coast$DEWP)
# total_east_coast$VIS <- as.numeric(total_east_coast$VIS)
# total_east_coast$TIDE <- as.numeric(total_east_coast$TIDE)
# total_east_coast$site <- as.character(total_east_coast$site)
# total_east_coast$Date <- as.numeric(total_east_coast$Date)

