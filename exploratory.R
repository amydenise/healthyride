##1.  IMPORTING DATA 
source("~/healthyride/funs.R")
data_original <- read.csv("~/healthyride/data/HealthyRideRentals_2015_Q3.csv", stringsAsFactors = FALSE)
data <- data_original
stations <- read.csv("~/healthyride/data/HealthyRideStations2015.csv", stringsAsFactors = FALSE)

#NOTE!:  Names in the stations file do not match the consistent names in the data file: 
#Ex: In the Station Name File "Forbes Ave & Market Sq"  is "Forbes Ave & Market Square" in the data file 
#Corrected by Hand in Excel, the following is a list of the official stations as used in "HealthyRideRentals_2015_Q3.csv"
corrected_stations <- read.csv("~/healthyride/data/corrected_names.csv", stringsAsFactors = FALSE)
corrected_names <- corrected_stations[,"Corrected_Names"]


##2.  DATA VALIDATION & TYPE CONVERSION
dim(data)  #40,083 x 10
head(data)
apply(data, 2, function(x) length(unique(x)))  #unique values per column
#TripId is unique
#492 Unique Bikes
#TripDuration is in seconds

#?1: 53 StationID but Stations file only contains 50 (numbered 1000-1049)
table(data$FromStationId, useNA = "always")
as.data.frame(subset(data, FromStationId == 1050)[,"FromStationName"])
as.data.frame(subset(data, FromStationId == 1051)[,"FromStationName"])
#The additional 3 are: "Healthy Ride Hub", "Transit", and missing
#Healthy Ride Hub & Transit were added to corrected_names

#?2:  Why are there more Station names than ID's?
#Answer: Some user input in ToStationName & FromStationName
sum(is.na(data$FromStationId))  #82
mismatched_from_rows <- data[-which(data$FromStationName %in% corrected_names),]
dim(mismatched_from_rows)
mismatched_from_rows$FromStationId  
#In FromStationName, all station names not in corrected_names are NA and every NA corresponds to a hand-coded name

sum(is.na(data$ToStationId))  #1356
mismatched_to_rows <- data[-which(data$ToStationName %in% corrected_names),]
dim(mismatched_to_rows)  #1349 rows have ToStationName not in corrected_names, 988 have no information in ToStationName
mismatched_to_rows$ToStationId  #& All station names not in corrected_names are NA 

data_to_matched <- data[which(data$ToStationName %in% corrected_names),]
subset(data_to_matched, is.na(data_to_matched$ToStationId))  #7 have ToStationName in corrected_names but no associated ToStationId

#Conclusion about StationIds and StationNames:  
#A small portion of FromStationId's (.2%) have been miscoded by hand and have FromStationId = NA as a result.
#Many of these could be likely be corrected, but since it is such as small number, we proceed by deleting them from
#the data set, at least for the time being.  
#A larger portion of ToStationIds are NA (3.38%), but most of these (988/1356 ~ 73%) contain ToStationName == "" and therefore
#cannot be adjusted.  Only .92% of the rows could be possibly recovered, so for the time being these 1356 rows are not corrected nor used.

#?forHRPgh:  Are these ToStationName = "" really missing?  All but one of them starts at Schenly Dr at Schenly Plaza
subset(subset(data,ToStationName == ""), FromStationName != "Schenley Dr at Schenley Plaza (Carnegie Library")

#Converting Date & Time of StartTime % StopTime to useable format
library(lubridate)
StartTimeP <- mdy_hm(data$StartTime)  #Convert StartTime to Posix
StopTimeP <- mdy_hm(data$StopTime) #Convert StopTime to Posix
TripDurationM <- round(data$TripDuration/60, 0)   #Convert TripDuration to Minutes

#created dataframe that removes NAs
data_no_na <- cbind(data, StartTimeP, StopTimeP, TripDurationM)  #adding columns
data_na_na <- data_no_na[complete.cases(data_no_na[,c("FromStationId", "ToStationId")]),] #Drop rows with ToStationId | FromStationId = NA
write.csv(data_no_na, file = "~/healthyride/data/data_no_na.csv")  #write to file

#create dataframe that removes NAs and StationID 1050 & 1051
data_no_5051 <- subset(data_no_na, FromStationId < 1050 & ToStationId < 1050)  #Removing rows with From or To stations == 1050 or 1051
write.csv(data_no_5051, file = "~/healthyride/data/datas_no_5051.csv")  #write to file

#Calculate Matrix counting trips start and end (bidirectional)
mat <- tripMatrix(stations = stations, data = data_no_5051)
write.csv(mat, file = "~/healthyride/data/mat.csv")  #write to file

#convert to matrix type for calculations
mat <- as.matrix(mat)

#Some stats about trip counts
tripCount <- nrow(data_no_5051)
stationCount <- length(mat)
percent((sum(diag(mat))/tripCount))  #% of trips with FromStationId == ToStationId: "32.02%"
sum(mat == 0)  #Count of Pairs with no trips: 699
percent(sum(mat == 0)/stationCount)  #% of pairs with no trips: "27.96%"




##4.  UNIVARIATE EXPLORATION

#this is broken since it is using "datas" == data_no_na...needs updated/decide which set to use

##########################Var: TripDurationM
barplot(table(datas$TripDurationM))
#need more granularity
barplot(table(subset(datas, TripDurationM < 300, select = "TripDurationM")))
#more still
barplot(table(subset(datas, TripDurationM < 180, select = "TripDurationM")))
summary(TripDurationM)
summary(subset(datas, TripDurationM < 180, select = "TripDurationM"))

#binning bigger
hist(subset(datas, TripDurationM < 180, select = "TripDurationM")[,1], xlab = "TripDurationM")
#Note: TripDurationM follows an exponential looking distribution

#Description of Ride Duration (minutes) by UserType
time_intervals <- c(2, 5, 30, 60, 90, 180, 720, max(TripDurationM))
durationDes(time_intervals)

#Thougts:
#~5% of rides are less than 2 minutes?  Are these people experiencing trouble?
#~50% of rides are < 1/2 hour (~1/2 customers, ~1/2 subscribers)
#An additional ~18% are > 30min, < 60 minutes (5:1 customers to Subscribers)
#A daily use costs $24.  It's more economical to get a daily pass if a ride last longer than 12 hours (720 mins)
#Over the summer nearly 500 people did not take advantage of the daily rates when it would have been better for them.  


