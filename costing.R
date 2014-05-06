#  Load packages, functions and QARMA sheet
library(plyr)
source.with.encoding('~/Documents/scripts/parts_and_numbers.R', encoding='UTF-8')
rep2X <- read.csv("~/Downloads/3QARMA Returns Database - Rep2X RMAs.csv")



###############################
#####     FORMAT DATA     #####
###############################



#  Change date format
rep2X[,6] <- as.Date(rep2X[,6], "%m/%d/%Y")

#  Get necessary columns
rep2X <- organize(rep2X)

#  Rename columns
colnames(rep2X) <- c("Date", "Week", "Month", "CS", "SR", "Action", "Makercare", "Repaired", "Shipping", "Labor", "Ticket")

#  Change makercare and repaired columns to standard format
rep2X$Makercare <- sub("y *", "Y", rep2X$Makercare, ignore.case = TRUE)
rep2X$Makercare <- sub("n *", "N", rep2X$Makercare, ignore.case = TRUE)

rep2X$Repaired <- sub("y *", "Y", rep2X$Repaired, ignore.case = TRUE)
rep2X$Repaired <- sub("n *", "N", rep2X$Repaired, ignore.case = TRUE)

#  Put all numbers in numeric format
rep2X$Shipping <- as.numeric(as.character(rep2X$Shipping))
rep2X$Labor <- as.numeric(as.character(rep2X$Labor))

#  Get all of the RMA's that were repairs
repairs <-  rep2X[grep("Y", rep2X$Repaired, ignore.case=TRUE),]
shipped <- rep2X[grep("replac(e)?ment", rep2X$Action, ignore.case=TRUE),]

#  Create list of containing makercare and non-makercare RMA's
wk1 <- makercare(rep2X)



###############################
#####     GET NUMBERS     #####
###############################



#  Get shipping costs, labor time, labor costs, and average shipping costs for makercare
#  and non-makercare bots
sapply(wk1, function(x) {
  ddply(x, .(Makercare), summarise,
        shippingCost = sum(Shipping, na.rm = TRUE),
        labor = sum(Labor, na.rm = TRUE),
        laborCost = sum(Labor, na.rm = TRUE) * 25,
        AvgShipCost = mean(Shipping, na.rm = TRUE))
})

#  Get number of repairs, labor time, labor costs, and average labor costs for 
#  repaired makercare and non-makercare bots
ddply(repairs, .(Makercare), summarise,
               repairs = length(Repaired %in% "Y"),
               laborTime = sum(Labor, na.rm = TRUE),
               laborCost = sum(Labor, na.rm = TRUE) * 25,
               AvgLaborCost = mean(Labor, na.rm = TRUE) * 25
        )

