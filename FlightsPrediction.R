## Load row data set
flights <- read.csv("/Users/vito/Documents/Github/R/FIT5142/DelayedFlights.csv", header = TRUE)


## Overview of the raw data

head(flights, 5) # list first 5 records
str(flights) # structure
summary(flights) # quick look


## Preliminary data processing

# we do not need index column
flights <- subset(flights, select = -X)

# Remove Year Column
# All flight records are in 2008
flights <- subset(flights, select = -Year)


#===================================================================================
#
#                               Monthly Flight Status
#
#===================================================================================


# we we do month group by
monthCount <- aggregate(x = flights$Month, by = list(flights$Month), FUN = "length")
# change column name
names(monthCount)[names(monthCount) == "Group.1"] <- "Month"
names(monthCount)[names(monthCount) == "x"] <- "Count"
monthCount$Month = factor(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                          levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

library(viridis)
library(plotly)
# bar chart
plot_ly(monthCount, x = ~Month, y = ~Count,
        type = "bar", text = ~Count, textposition = 'auto',
        insidetextfont = list(color = '#FFFFFF'),
        marker = list(color = seq(1, 24), colorscale='Viridis',
                      line = list(color = '#12005e', width = 1.5))) %>%
  layout(title = "Total Number of Flight of Each Month",
         xaxis = list(title = "Month", autotick = F, dtick = 1),
         yaxis = list(title = "Number of Flight"))

# Sort monthCount based on Count in descending order
orderedMonthCount <- monthCount[rev(order(monthCount$Count)), ]

# pie chart
plot_ly(orderedMonthCount, labels = ~Month, values = ~Count,
        type = "pie", sort = F, direction = "clockwise",
        textinfo = 'label+percent', showlegend = F,
        marker = list(colors = viridis(24),
                      line = list(color = '#FFFFFF', width = 1)),
        insidetextfont = list(color = '#FFFFFF')) %>%
  layout(title = 'Percentage of Each Month',
       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


#===================================================================================
#
#                               Flight Delay Status
#
#===================================================================================

flightsStatus <- data.frame("Status" = c("Delayed", "OnTime", "Cancelled", "Diverted"),
                            "Count" = c(nrow(flights[flights$ArrDelay >= 15, ]),
                                        nrow(flights[flights$ArrDelay < 15, ]),
                                        nrow(flights[flights$Cancelled == 1, ]),
                                        nrow(flights[flights$Diverted == 1, ])))

str(flightsStatus)
orderedflightsStatus <- flightsStatus[order(flightsStatus$Count), ]
orderedflightsStatus$Status <- factor(orderedflightsStatus$Status, levels = c("Cancelled", "Diverted", "OnTime", "Delayed"))

# bar chart
plot_ly(orderedflightsStatus, x = ~Status, y = ~Count,
        type = "bar", text = ~Count, textposition = 'auto',
        insidetextfont = list(color = '#FFFFFF'),
        marker = list(color = seq(1, 24), colorscale='Viridis',
                      line = list(color = '#12005e', width = 1.5))) %>%
  layout(title = "Different Flight Status",
         xaxis = list(title = "Status", autotick = F, dtick = 1),
         yaxis = list(title = "Number of Flight"))

# pie chart
plot_ly(orderedflightsStatus, labels = ~Status, values = ~Count,
        type = "pie", sort = T,
        textinfo = 'label+percent', showlegend = F,
        marker = list(colors = viridis(24),
                      line = list(color = '#FFFFFF', width = 1)),
        insidetextfont = list(color = '#FFFFFF')) %>%
  layout(title = 'Pie Chart of Different Flight Status',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


#===================================================================================
#
#                                 Cancelled flights
#
#===================================================================================

cancelledFlights <- flights[flights$CancellationCode != 'N', ]

cancelledFlights <- data.frame("Reason" = c("Weather", "Carrier", "NAS", "Security"),
                               "Count" = c(nrow(cancelledFlights[cancelledFlights$CancellationCode == 'B', ]),
                                           nrow(cancelledFlights[cancelledFlights$CancellationCode == 'A', ]),
                                           nrow(cancelledFlights[cancelledFlights$CancellationCode == 'C', ]),
                                           nrow(cancelledFlights[cancelledFlights$CancellationCode == 'D', ])))

cancelledFlights$Reason <- factor(cancelledFlights$Reason, levels = c("Weather", "Carrier", "NAS", "Security"))

# bar chart
plot_ly(cancelledFlights, x = ~Reason, y = ~Count,
        type = "bar", text = ~Count, textposition = 'auto',
        insidetextfont = list(color = '#FFFFFF'),
        marker = list(color = seq(1, 4), colorscale='Viridis',
                      line = list(color = '#000000', width = 2))) %>%
  layout(title = "Different Cancellation Reasons",
         xaxis = list(title = "Reason", autotick = F, dtick = 1),
         yaxis = list(title = "Number of Flight"))

# pie chart
plot_ly(cancelledFlights, labels = ~Reason, values = ~Count,
        type = "pie", sort = T,
        textinfo = 'label+percent', showlegend = T,
        marker = list(colors = viridis(4),
                      line = list(color = '#FFFFFF', width = 1)),
        insidetextfont = list(color = '#FFFFFF')) %>%
  layout(title = 'Pie Chart of Different Cancellation Reasons',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


#===================================================================================
#
#                                 Delayed flights
#
#===================================================================================

delayedFlights <- flights[flights$ArrDelay >= 15, ]

plot_ly(delayedFlights, y = ~ArrDelay,
        type = 'violin',
        box = list(visible = T),
        meanline = list(visible = T),
        line = list(color = viridis(1)),
        x0 = 'Delay time (in minutes)') %>% 
  layout(yaxis = list(title = "",zeroline = F))



#===================================================================================
#
#                             Processing missing value
#
#===================================================================================

summary(flights)

table(flights$Cancelled) # 633 cancelled
table(flights$Diverted) # 7754 diverted
summary(flights$ArrDelay) # 8387 NA ArrDelay = 7754 + 633

# Remove cancelled flights, we do not consider cancelled flights
# how many flights are cancelled: 633
nrow(subset(flights, Cancelled == 1))
# remove cancelled flights records and Cancelled Column
flights <- subset(flights, Cancelled == 0, select = -Cancelled)
flights <- subset(flights, select = -CancellationCode)

# Remove diverted flights, we do not consider diverted flights
# how many flights are diverted: 7754
nrow(subset(flights, Diverted ==1))
# remove diverted flights records and Diverted Column
flights <- subset(flights, Diverted == 0, select = -Diverted)

summary(flights)

# For CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay
# Remove these attributes
# ArrDelay = CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay
flights <- subset(flights, select = -c(CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay))

# For now, there is no missing value in the data set.
summary(flights)



#===================================================================================
#
#                             Processing data structure
#
#===================================================================================

# structure of data set
str(flights)

# convert FlightNum data structure to Character
flights$FlightNum <- as.character(flights$FlightNum)

# TailNum should not be Factor
# convert to Character
flights$TailNum <- as.character(flights$TailNum)

# Convert DayOfWeek to Factor
flights$DayOfWeek <- as.factor(flights$DayOfWeek)

# convert all time attributes from numeric to integer
flights$DepTime <- as.integer(flights$DepTime)
flights$ArrTime <- as.integer(flights$ArrTime)
flights$ActualElapsedTime <- as.integer(flights$ActualElapsedTime)
flights$CRSElapsedTime <- as.integer(flights$CRSElapsedTime)
flights$AirTime <- as.integer(flights$AirTime)

# check structure
str(flights)


#===================================================================================
#
#                               Processing data
#
#===================================================================================


# consider those flights that delay time greater or equal to 15 minutes as delayed flights
# add a new column to indicate whether the flight delayed or not. (1:Delayed, 2: Not Delayed)
flights$IsDelayedFlight <- as.integer(ifelse(flights$ArrDelay >= 15, 1, 0))

# check structure
str(flights)

# how many rows of TailNum is ""
nrow(flights[flights$TailNum == "", ]) # 3
# Replace empty cabins with a "U"
flights[which(flights$TailNum == ""), "TailNum"] <- "U"
# Take a look at just the first char as a factor
TailNumFirstChar <- as.factor(substr(flights$TailNum, 1, 1))
str(TailNumFirstChar)
table(TailNumFirstChar)
# add to flights data set
flights$TailNumFirstChar <- TailNumFirstChar

# how many rows of FlightNum is ""
nrow(flights[flights$FlightNum == "", ]) # 0
FlightNumFirstChar <- as.factor(substr(flights$FlightNum, 1, 1))
str(FlightNumFirstChar)
table(FlightNumFirstChar)
flights$FlightNumFirstChar <- FlightNumFirstChar


#===================================================================================
#
#                     Split categorical data and numerical data 
#
#===================================================================================

str(flights)

# Numerical data
flightsNumerical <- subset(flights, select = c(DayofMonth, DepTime, CRSDepTime, ArrTime, CRSArrTime, ActualElapsedTime, CRSElapsedTime, AirTime, ArrDelay, DepDelay, Distance, TaxiIn, TaxiOut, IsDelayedFlight))
str(flightsNumerical)

# Correlation Matrix
plot_ly(x = names(flightsNumerical),
        y = names(flightsNumerical),
        z = cor(flightsNumerical),
        colors = viridis(24),
        type = "heatmap") %>%
  layout(title = "Correlation Matrix Heat Map of Numerical Data")


# Categorical data
flights$IsDelayedFlight <- as.factor(flights$IsDelayedFlight)
flightsCategorical <- subset(flights, select = c(Month, DayOfWeek, UniqueCarrier, Origin, Dest, TailNumFirstChar, FlightNumFirstChar, IsDelayedFlight))
str(flightsCategorical)

# Chi-Squared Test
tmp = as.data.frame(cbind(NotDelay = table(flightsCategorical$TailNumFirstChar, flightsCategorical$IsDelayedFlight)[,1], Delay = table(flightsCategorical$TailNumFirstChar, flightsCategorical$IsDelayedFlight)[,2]))

chisq.test(table(flightsCategorical$Month, flightsCategorical$IsDelayedFlight))
chisq.test(table(flightsCategorical$DayOfWeek, flightsCategorical$IsDelayedFlight))
chisq.test(table(flightsCategorical$UniqueCarrier, flightsCategorical$IsDelayedFlight))
chisq.test(table(flightsCategorical$Origin, flightsCategorical$IsDelayedFlight))
chisq.test(table(flightsCategorical$Dest, flightsCategorical$IsDelayedFlight))
chisq.test(table(flightsCategorical$TailNumFirstChar, flightsCategorical$IsDelayedFlight))
chisq.test(table(flightsCategorical$FlightNumFirstChar, flightsCategorical$IsDelayedFlight))


#===================================================================================
#
#                                 Random Forest
#
#===================================================================================

# Load package
library(randomForest)

# Define train sample count
trainSampleNumber <- 120000

# reshuffle dataframe
reshuffledFlights <- cbind(flightsCategorical,flightsNumerical)
reshuffledFlights <- reshuffledFlights[sample(nrow(reshuffledFlights)), ]

# define labels
rf.label <- reshuffledFlights[1:trainSampleNumber, "IsDelayedFlight"]

# Train a Random Forest using Month & DayOfWeek
# error rate: 35.12%
# Confusion matrix:
#   0     1 class.error
# 0 0 42146           1
# 1 0 77854           0
rf.train.1 <- reshuffledFlights[1:trainSampleNumber, c('Month', 'DayOfWeek')]
set.seed(428)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = T, ntree = 1000)
rf.1
varImpPlot(rf.1)

# "UniqueCarrier" & "DayOfWeek"
# error rate: 35.13%
# Confusion matrix:
#    0     1  class.error
# 0  7 42139 0.9998339107
# 1 13 77841 0.0001669792
rf.train.2 <- reshuffledFlights[1:trainSampleNumber, c('UniqueCarrier', 'DayOfWeek')]
set.seed(428)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = T, ntree = 1000)
rf.2
varImpPlot(rf.2)

# "UniqueCarrier", "DayOfWeek", "Month"
# error rate: 34.77%
# Confusion matrix:
#      0     1 class.error
# 0 1997 40149  0.95261709
# 1 1569 76285  0.02015311
rf.train.3 <- reshuffledFlights[1:trainSampleNumber, c('UniqueCarrier', 'DayOfWeek', 'Month')]
set.seed(428)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = T, ntree = 1000)
rf.3
varImpPlot(rf.3)

# 'UniqueCarrier', 'DayOfWeek', 'Month', "DepTime", "DepDelay", "TaxiIn", "TaxiOut"
# time: 7:28
# error rate: 12.49%
# Confusion matrix:
#       0     1 class.error
# 0 34085  8061  0.19126370
# 1  6926 70928  0.08896139
rf.train.4 <- reshuffledFlights[1:trainSampleNumber, c('UniqueCarrier', 'DayOfWeek', 'Month', "DepTime", "DepDelay", "TaxiIn", "TaxiOut")]
set.seed(428)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = T, ntree = 1000)
rf.4
varImpPlot(rf.4)

# 'UniqueCarrier', 'DayOfWeek', 'Month', "DepTime", "DepDelay", "CRSDepTime", "TaxiOut"
# time: 7:26
# error rate: 13.19%
# Confusion matrix:
#       0     1 class.error
# 0 34068  8078  0.19166706
# 1  7746 70108  0.09949392

rf.train.5 <- reshuffledFlights[1:trainSampleNumber, c('UniqueCarrier', 'DayOfWeek', 'Month', "DepTime", "DepDelay", "CRSDepTime", "TaxiOut")]
set.seed(428)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = T, ntree = 1000)
rf.5
varImpPlot(rf.5)







str(reshuffledFlights)

flightsDataSet <- reshuffledFlights[1:100000, c("UniqueCarrier", "DayOfWeek", "Month", "DepTime", "DepDelay", "CRSDepTime", "TaxiOut", "IsDelayedFlight")]
flightsDataSet$IsDelayedFlight <- as.numeric(flightsDataSet$IsDelayedFlight)

str(flightsDataSet)

flightsDataSet$DayOfWeek <- factor(flightsDataSet$DayOfWeek,
                                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                                   labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

flightsDataSet$Month <- factor(flightsDataSet$Month,
                               levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                               labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))



#===================================================================================
#
#                              One-Hot Encoding
#
#===================================================================================

length_FlightsDataSet <- nrow(flightsDataSet)

# DayOfWeek
levels_DayOfWeek <- length(levels(flightsDataSet$DayOfWeek))
OneHot_DayOfWeek <- as.data.frame(matrix(numeric(0), ncol = levels_DayOfWeek))

for (i in 1:levels_DayOfWeek) {
  colnames(OneHot_DayOfWeek)[i] <- paste("is_", levels(flightsDataSet$DayOfWeek)[i], sep = "")
}

# 700,000 iterations
for (i in 1:length_FlightsDataSet) {
  value = flightsDataSet[i, "DayOfWeek"]
  for (j in 1:levels_DayOfWeek) {
    if (paste("is_", value, sep = "") == colnames(OneHot_DayOfWeek)[j]) {
      OneHot_DayOfWeek[i, j] = 1
    }
    else {
      OneHot_DayOfWeek[i, j] = 0
    }
  }
}

str(OneHot_DayOfWeek)


# Month
levels_Month <- length(levels(flightsDataSet$Month))
OneHot_Month <- as.data.frame(matrix(numeric(0), ncol = levels_Month))

for (i in 1:levels_Month) {
  colnames(OneHot_Month)[i] <- paste("is_", levels(flightsDataSet$Month)[i], sep = "")
}

# 1,200,000 iterations
# time : 38 mins
for (i in 1:length_FlightsDataSet) {
  value = flightsDataSet[i, "Month"]
  for (j in 1:levels_Month) {
    if (paste("is_", value, sep = "") == colnames(OneHot_Month)[j]) {
      OneHot_Month[i, j] = 1
    }
    else {
      OneHot_Month[i, j] = 0
    }
  }
}

str(OneHot_Month)


# UniqueCarrier
levels_UniqueCarrier <- length(levels(flightsDataSet$UniqueCarrier))
OneHot_UniqueCarrier <- as.data.frame(matrix(numeric(0), ncol = levels_UniqueCarrier))

for (i in 1:levels_UniqueCarrier) {
  colnames(OneHot_UniqueCarrier)[i] <- paste("is_", levels(flightsDataSet$UniqueCarrier)[i], sep = "")
}

# 2,000,000 iterations
# time: 53 mins
for (i in 1:length_FlightsDataSet) {
  value = flightsDataSet[i, "UniqueCarrier"]
  for (j in 1:levels_UniqueCarrier) {
    if (paste("is_", value, sep = "") == colnames(OneHot_UniqueCarrier)[j]) {
      OneHot_UniqueCarrier[i, j] = 1
    }
    else {
      OneHot_UniqueCarrier[i, j] = 0
    }
  }
}

str(OneHot_UniqueCarrier)


# UniqueCarrier
levels_IsDelayedFlight <- length(levels(flightsDataSet$IsDelayedFlight))
OneHot_IsDelayedFlight <- as.data.frame(matrix(numeric(0), ncol = levels_IsDelayedFlight))

for (i in 1:levels_IsDelayedFlight) {
  colnames(OneHot_IsDelayedFlight)[i] <- paste("is_", ifelse(levels(flightsDataSet$IsDelayedFlight)[i] == "0", "Delayed", "NotDelayed"), sep = "")
}


# 200,000 iterations
# time: 17 mins
for (i in 1:length_FlightsDataSet) {
  value = flightsDataSet[i, "IsDelayedFlight"]
  for (j in 1:levels_IsDelayedFlight) {
    if (value == ifelse(colnames(OneHot_IsDelayedFlight)[j] == "is_Delayed", 1, 0)) {
      OneHot_IsDelayedFlight[i, j] = 1
    }
    else {
      OneHot_IsDelayedFlight[i, j] = 0
    }
  }
}

str(OneHot_IsDelayedFlight)


#===================================================================================
#
#                                    Combine
#
#===================================================================================

OneHot_Flights_DataSet <- cbind(flightsDataSet, OneHot_DayOfWeek, OneHot_Month, OneHot_UniqueCarrier)
str(OneHot_Flights_DataSet)
Flights_DataSet <- subset(OneHot_Flights_DataSet, select = -c(UniqueCarrier, DayOfWeek, Month, IsDelayedFlight))

str(Flights_DataSet)

Flights_DataSet$DepTime = scale(Flights_DataSet$DepTime, center = T, scale = T)
Flights_DataSet$DepDelay = scale(Flights_DataSet$DepDelay, center = T, scale = T)
Flights_DataSet$CRSDepTime = scale(Flights_DataSet$CRSDepTime, center = T, scale = T)
Flights_DataSet$TaxiOut = scale(Flights_DataSet$TaxiOut, center = T, scale = T)

str(Flights_DataSet)

#===================================================================================
#
#                                 Output Data
#
#===================================================================================

# Flights_Target
write.csv(OneHot_IsDelayedFlight, "/Users/vito/Desktop/Flights_Target.csv", row.names = FALSE)

# Flights_DataSet
write.csv(Flights_DataSet, "/Users/vito/Desktop/Flights_DataSet.csv", row.names = FALSE)




