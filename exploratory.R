data <- read.csv("~/healthyride/data/HealthyRideRentals_2015_Q3.csv")
stations <- read.csv("~/healthyride/data/HealthyRideStations2015.csv")

#######checking import/data validation######
dim(data)  #40,083 x 10
head(data)
apply(data, 2, function(x) length(unique(x)))  #unique values per column
#D1: TripId is unique

#?: 53 Stations but Stations file only contains 50 (numbered 1000-1049)
table(data$FromStationId, useNA = "always")
#D2: Some FromStationId

from_counts <- table(data$FromStationId)
barplot(from_counts, main="FromStationId Distribution", horiz=TRUE)

from_counts
