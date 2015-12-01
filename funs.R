#Trips Matrix
#Matrix counting trips start and end (bi-directional)
tripMatrix <- function(stations, data) {
  #Initatize of matrix of 0s
  dim = length(stations$StationNum)
  mat <- as.data.frame(matrix(data = rep(0, 2500),nrow = dim, ncol = dim))
  rownames(mat) <- stations$StationNum
  colnames(mat) <- stations$StationNum
  
  for (i in 1:nrow(data)){
    From_ID_char <- as.character(data$FromStationId[i])
    To_ID_char <- as.character(data$ToStationId[i])
    mat[To_ID_char, From_ID_char] <- mat[To_ID_char, From_ID_char] + 1
  }
  return(mat)
}


#Percentage Formatter
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#TripDurationM Descriptive Table Creation by Usertype
durationDes <- function(x){  
  min_breaks <- x
  len <- length(min_breaks)
  per <- rep(NA, len)  
  df <- data.frame(matrix(ncol = 7, nrow = len))
  colnames(df) <- c("Rides Duration (minutes)", "%", "Cumulative %", "Customer", "Subscriber", "Daily", "No Info")
  #calculating percentages
  for (i in 1:len){
    per[i] <- nrow(subset(datas, TripDurationM < min_breaks[i]))/nrow(datas)
    df[i, 3] <- percent(per[i], 1)
    if (i == 1) {
      df[i, 1] <- sprintf("0 - %d minutes", min_breaks[i])
      df[i, 2] <- df[i,3]
      temp <- as.data.frame(table(subset(datas, TripDurationM < min_breaks[i], select = "UserType")))
      existings <- as.character(temp[,1])
      temp[,1] <- as.character(temp[,1])
      df[i, 4] <- ifelse ("Customer" %in% existings, subset(temp, Var1 == "Customer")[,2], 0)
      df[i, 5] <- ifelse("Subscriber" %in% existings, subset(temp, Var1 == "Subscriber")[,2], 0)
      df[i, 6] <- ifelse("Daily" %in% existings, subset(temp, Var1 == "Daily")[,2], 0)
      df[i, 7] <- ifelse("" %in% existings, subset(temp, Var1 == ""[,2]), 0)}
    else {
      df[i, 1] <- sprintf("%d - %d minutes", (min_breaks[i-1]+1), min_breaks[i])
      df[i, 2] <- percent(per[i] - per[i-1], 1)
      temp <- as.data.frame(table(subset(datas, (TripDurationM > min_breaks[i-1]) & 
                                           (TripDurationM < min_breaks[i]), select = "UserType")))
      existings <- as.character(temp[,1])
      temp[,1] <- as.character(temp[,1])
      df[i, 4] <- ifelse ("Customer" %in% existings, subset(temp, Var1 == "Customer")[,2], 0)
      df[i, 5] <- ifelse("Subscriber" %in% existings, subset(temp, Var1 == "Subscriber")[,2], 0)
      df[i, 6] <- ifelse("Daily" %in% existings, subset(temp, Var1 == "Daily")[,2], 0)
      df[i, 7] <- ifelse("" %in% existings, subset(temp, Var1 == "")[,2], 0)}
  }
return(df)
}