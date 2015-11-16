##1.  IMPORTING DATA 
data <- read.csv("~/healthyride/data/HealthyRideRentals_2015_Q3.csv", stringsAsFactors = FALSE)
stations <- read.csv("~/healthyride/data/HealthyRideStations2015.csv", stringsAsFactors = FALSE)

#NOTE!:  Names in the stations file do not match the consistent names in the data file: 
#Ex: In the Station Name File "Forbes Ave & Market Sq"  is "Forbes Ave & Market Square" in the data file 
#Corrected by Hand in Excel, the following is a list of the official stations as used in "HealthyRideRentals_2015_Q3.csv"
corrected_stations <- read.csv("~/healthyride/data/corrected_names.csv", stringsAsFactors = FALSE)
corrected_names <- corrected_stations[,"Corrected_Names"]
attach(data)

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
TripDurationM <- TripDuration/60   #Convert TripDuration to Minutes

data <- cbind(data, StartTimeP, StopTimeP, TripDurationM)  #adding columns
data <- data[complete.cases(data[,c("FromStationId", "ToStationId")]),] #Drop rows with ToStationId | FromStationId = NA

##3.  UNIVARIATE EXPLORATION