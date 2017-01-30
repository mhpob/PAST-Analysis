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
total_east_coast[[9]]$site <- names(total_east_coast[9])


# 2) rbind (do.call)

# 3) take care of first row
# 4) tell R that 999 is NA
        # is.na?
        # maybe should have done during input?
        # just google it
# 5) Get dates right: paste() columns into new, ?lubridate::ymd_hm
# 6) sort out classes of other columns: as.WHATEVERTHECLASSSHOULDBE
###   data$numeric <- as.numeric(data$numeric)