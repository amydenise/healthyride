##1.  IMPORTING DATA 
source("~/healthyride/funs.R")      #My functions file
data_original <- read.csv("~/healthyride/data/HealthyRideRentals_2015_Q3.csv", stringsAsFactors = FALSE)  #Clean copy of data
data <- data_original     #WORKING copy of data
stations <- read.csv("~/healthyride/data/HealthyRideStations2015.csv", stringsAsFactors = FALSE)  #HealthyRide's Stations List
travel_times <- read.csv("~/healthyride/data/transport_times.csv", stringsAsFactors = FALSE)  #Times extracted from Google API
colnames(travel_times) <- c("FromStationId", "ToStationId", "TimeInSec")    #clarifying column names in Google time file

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


##2.1  Why more StationIds than Stations in the stations file?  
#53 StationID but Stations file only contains 50 (numbered 1000-1049)
table(data$FromStationId, useNA = "always")
as.data.frame(subset(data, FromStationId == 1050)[,"FromStationName"])
as.data.frame(subset(data, FromStationId == 1051)[,"FromStationName"])
#The additional 3 are: "Healthy Ride Hub", "Transit", and missing
#Healthy Ride Hub & Transit were added to corrected_names
#Note: After checking with HealthyRide Pittsburgh, Healthy Ride Hub is their office in the strip, and transit
#is a catchall for any other location. 


##2.2  Why are there more Station names in the data file than ID's?
#Answer: Some user input in ToStationName & FromStationName, making them inconsistent!
#When a name not matching a station ID is input, the system places an NA in ID column

#FromStation:
sum(is.na(data$FromStationId))  #82 missing FromStationIds
mismatched_from_rows <- data[-which(data$FromStationName %in% corrected_names),]  
dim(mismatched_from_rows)    #82 rows with FromStationName not recognized
mismatched_from_rows$FromStationId  # All 82 rows with station names not in corrected_names are NA 

#ToStation:
sum(is.na(data$ToStationId))  #1356 missing ToStationId
mismatched_to_rows <- data[-which(data$ToStationName %in% corrected_names),]
dim(mismatched_to_rows)  #1349 rows have ToStationName not in corrected_names
dim(subset(mismatched_to_rows, mismatched_to_rows$ToStationName == "")) # 988 of these have ToStationName == ""
mismatched_to_rows$ToStationId  # All 1349 rows with station names not in corrected_names are NA
data_to_matched <- data[which(data$ToStationName %in% corrected_names),]
subset(data_to_matched, is.na(data_to_matched$ToStationId))  #7 have ToStationName in corrected_names but no associated ToStationId
#1349 + 7 = 1356 missings


##2.3  Removing rows with ToStationID == NA | FromStationID == NA
#A small portion of FromStationId's (.2%) have been miscoded by hand and have FromStationId = NA as a result.
#Many of these could be likely be corrected, but since it is such as small number, we proceed by deleting them from
#the data set, at least for the time being.  
temp <- subset(data, !is.na(data$FromStationId))  #removing 82 rows with FromStationID == NA

#A larger portion of ToStationIds are NA (3.38%), but most of these (988/1356 ~ 73%) contain ToStationName == "" and therefore
#cannot be adjusted.  Only .92% of the rows could be possibly recovered, so for the time being these 1356 rows are deleted.
data <- subset(temp, !is.na(temp$ToStationId))  #removing rows with ToStationID == NA

#Note: 22 rows have both ToStationId and FromStationId not in corrected_name
dim(mismatched_to_rows[which(mismatched_to_rows$TripId %in% mismatched_from_rows$TripId),]) 

#Therefore, After deleting we expect: 40083 - 82 - 1356 + 22 = 38667 rows:
dim(data)  #match!

#?forHRPgh:  Are these ToStationName = "" really missing?  All but one of them starts at Schenly Dr at Schenly Plaza
subset(subset(data,ToStationName == ""), FromStationName != "Schenley Dr at Schenley Plaza (Carnegie Library")


##2.4  Converting Date & Time of StartTime % StopTime to useable format, appending to data
library(lubridate)
StartTimeP <- mdy_hm(data$StartTime)  #Convert StartTime to Posix
StopTimeP <- mdy_hm(data$StopTime) #Convert StopTime to Posix
TripDurationM <- round(data$TripDuration/60, 0)   #Convert TripDuration to Minutes

#Adding StartTimeP, StopTimeP, & TripDurationM to working data frame
data <- cbind(data, StartTimeP, StopTimeP, TripDurationM)  #adding columns
write.csv(data, file = "~/healthyride/data/data_no_na.csv")  #write to file jic

##2.5 Removing StationID == 1050 | 1051 from data
data <- subset(data, FromStationId < 1050 & ToStationId < 1050)  #Removing rows with From or To stations == 1050 or 1051
write.csv(data, file = "~/healthyride/data/data_no_na_no_50_no_51.csv")  #write to file jic
dim(data)  #38221, 446 rows remoced

#data IS THE FINAL WORKING DATA SET at this point

##3.  TRIPS HAVE A START POINT AND DESTINATION.  WHAT ARE MOST/LEAST USED?  
#Calculate Matrix counting trips start and end (bidirectional)
mat <- tripMatrix(stations = stations, data = data)
write.csv(mat, file = "~/healthyride/data/mat.csv")  #write to file

#convert to matrix type for calculations
mat <- as.matrix(mat)

#Some exploration about trip counts
tripCount <- nrow(data)
stationCount <- length(mat)
percent((sum(diag(mat))/tripCount))  #% of trips with FromStationId == ToStationId: "32.02%"
sum(mat == 0)  #Count of Station Pairs with no trips: 699
percent(sum(mat == 0)/stationCount)  #% of pairs with no trips: "27.96%"  (699/2500)









