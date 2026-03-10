install.packages(c("dplyr","lubridate","ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A")
metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")
sensorLog <- read.csv("/cloud/project/activity04/Sensor log.csv",
                      na.strings = "#N/A")

#Removing unreliable data (adjusting clock inaccuracies, time zones)
# parse date
weather$dateF <- mdy_hm(weather$Date)
weather$dateET <- mdy_hm(weather$Date, tz="America/New_York") #adjusting UTC to EST
# create a day column
weather$doy <- yday(weather$dateF)
#month column
weather$month <- month(weather$dateF)
# create a year column
weather$year <- year(weather$dateF)

# examine precipitation using a bar plot
ggplot(data=weather[weather$doy > 121 & weather$doy < 274 ,],
       aes(x=dateF,
           y=Precip))+
  geom_col(color="royalblue4")+
  theme_classic()

#Adding Precipitation column to convert May and June to NA
weather$precip.QC <- ifelse(weather$doy >= 121 & weather$doy <= 188 & weather$year == 2021, 
                            # evaluate if the doy is between May 1 and July 7 2021
                            NA, # value if true
                            weather$Precip) # value if false: uses original precipitation observation

#Creating flags
weather$FreezeFlag <- ifelse(weather$AirTemp <= 0, # check if at or below zero
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero

#Functions to Check Data (time intervals)
weather$dateF[2] %--% weather$dateF[3]
int_length(weather$dateF[2] %--% weather$dateF[3]) #900 second intervals between observations

test <- weather$dateF[1:10]
test
test[-1] #removes first observation from test vector (starts from observation 2)

#Checking if 900 second interval length between observations
timeCheck900 <- function(x) {
  intervals <- x[] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
} 
timeCheck900(weather$dateF)

#Set up variable to be used in For Loops
soilFiles <- list.files("/cloud/project/activity04/soil")
soilList <- list()

for(i in 1:length(soilFiles)){
  soilList[[i]] <- read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
}
# inspect the begining of the first file
head(soilList[[1]])
# get info about your list
str(soilList)

soilData <- do.call("rbind", soilList)
str(soilData)

#Calculating Moving Average (MA)
airMA <- numeric()
for(i in 8:length(weather$AirTemp)){ #Have to start on 8th observation for MA
  airMA[i] <- mean(weather$AirTemp[(i-7):i])
}

weather$airMA <- airMA

#Plot for Moving Average
ggplot()

#Homeowork

#Question 1
sum(is.na(weather$Precip))

#Question 2
weather$VoltageFlag <- ifelse(weather$BatVolt <= 8.5, 1, 0) 

#Question 3
weather$UnrealisticRadFlag <- ifelse(weather$SolRad >= 1200 | weather$SolRad < 0,1,0)
weather$UnrealisticTempFlag <- ifelse(weather$AirTemp <= -34 | weather$AirTemp >= 35,1,0)

#Question 4
wintermonths <- c("1", "2", "3")
wintertemp <- weather %>%
  filter(month %in% wintermonths & year == "2021")
ggplot(wintertemp, aes(x=dateF, y=AirTemp))+
  geom_line()+
  labs(title="Clinton Winter Air Temperatures", x="Date", y="Air Temperature (celsius)")+
  theme_minimal()

#Question 5
precipFiles <- list.files("/cloud/project/activity04")
totdailyprecip <- list()

marchapril <- c("3","4")
maraprtemp <- weather %>%
  filter(month %in% marchapril & year == "2021")
maraprtemp$fahr <- maraprtemp$AirTemp * 1.8 + 32
for(i in 2:nrow(maraprtemp)){
 if (maraprtemp$fahr[i] < 35 | maraprtemp$fahr[i] < 35) {
   maraprtemp$Precip[i] <- NA
 }
}

sum(!is.na(maraprtemp$Precip))

#Question 6
soilData$dateF <- mdy_hm(soilData$Timestamp)
soilData$dateET <- mdy_hm(soilData$Timestamp, tz="America/New_York") #adjusting UTC to EST

int_length(soilData$Timestamp[1] %--% soilData$Timestamp[2])
intervals <- soilData$Timestamp[-length(soilData$Timestamp)] %--% soilData$Timestamp[-1]
timeCheck3600 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 3600]
}
# run on soil data
timeCheck3600(soilData$Timestamp)

