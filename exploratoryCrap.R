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

