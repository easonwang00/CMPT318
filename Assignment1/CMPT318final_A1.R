
#install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.csv(file = "Group_Assignment_1_Dataset.txt", header = TRUE)
#install.packages("ggcorrplot")
#install.packages("gglot2")
library(ggplot2)
library(ggcorrplot)
#install.packages("data.table")
library(data.table)
#install.packages("hms")
library(hms)


#partA
df$Date <- as.POSIXlt(df$Date,format = "%d/%m/%Y")
df$Time <- as.POSIXlt(df$Time,format = "%H:%M:%S")
df <- subset(df, df$Date >= as.POSIXlt("7/5/2007",format = "%d/%m/%Y") & df$Date <= as.POSIXlt("13/5/2007",format = "%d/%m/%Y"))
arithmetic_mean_A <- mean( df$Global_active_power )
arithmetic_mean_B <- mean( df$Global_reactive_power )
arithmetic_mean_C <- mean( df$Voltage )
geometric_mean_A <- exp(mean(log( df$Global_active_power )))
geometric_mean_B <- exp(mean(log( df$Global_reactive_power )))
geometric_mean_C <- exp(mean(log( df$Voltage )))
median_A <- median( df$Global_active_power )
median_B <- median( df$Global_reactive_power )
median_C <- median( df$Voltage )

getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_A <- getMode( df$Global_active_power )
mode_B <- getMode( df$Global_reactive_power )
mode_C <- getMode( df$Voltage )

standard_deviation_A <- sd( df$Global_active_power )
standard_deviation_B <- sd( df$Global_reactive_power )
standard_deviation_C <- sd( df$Voltage )

#subset weekedays and weekend
df_weekdays <- subset(df, df$Date$wday >= 1 & df$Date$wday <= 5)
df_weekend <- subset(df, df$Date$wday ==6 | df$Date$wday == 0)

# I choose 6AM-9AM as the day hours, and 9PM - 12AM as the night hours
df_weekdays_day <- subset(df_weekdays, df_weekdays$Time$hour >= 6 & df_weekdays$Time$hour < 9)
df_weekdays_night <- subset(df_weekdays, df_weekdays$Time$hour >= 21 | df_weekdays$Time$hour < 24)
df_weekend_day <- subset(df_weekend, df_weekend$Time$hour >= 6 & df_weekend$Time$hour < 9)
df_weekend_night <- subset(df_weekend, df_weekend$Time$hour >= 21 | df_weekend$Time$hour < 24)

min_df_weekdays_day_A <- min(df_weekdays_day$Global_active_power)
min_df_weekdays_day_B <- min(df_weekdays_day$Global_reactive_power)
min_df_weekend_day_A <- min(df_weekend_day$Global_active_power)
min_df_weekend_day_B <- min(df_weekend_day$Global_reactive_power)
min_df_weekdays_night_A <- min(df_weekdays_night$Global_active_power)
min_df_weekdays_night_B <- min(df_weekdays_night$Global_reactive_power)
min_df_weekend_night_A <- min(df_weekend_night$Global_active_power)
min_df_weekend_night_B <- min(df_weekend_night$Global_reactive_power)

max_df_weekdays_day_A <- max(df_weekdays_day$Global_active_power)
max_df_weekdays_day_B <- max(df_weekdays_day$Global_reactive_power)
max_df_weekend_day_A <- max(df_weekend_day$Global_active_power)
max_df_weekend_day_B <- max(df_weekend_day$Global_reactive_power)
max_df_weekdays_night_A <- max(df_weekdays_night$Global_active_power)
max_df_weekdays_night_B <- max(df_weekdays_night$Global_reactive_power)
max_df_weekend_night_A <- max(df_weekend_night$Global_active_power)
max_df_weekend_night_B <- max(df_weekend_night$Global_reactive_power)

#partB
cor_AB <- cor(df$Global_active_power,df$Global_reactive_power,method="pearson")
cor_AC <- cor(df$Global_active_power,df$Voltage,method="pearson")
cor_AD <- cor(df$Global_active_power,df$Global_intensity,method="pearson")
cor_AE <- cor(df$Global_active_power,df$Sub_metering_1,method="pearson")
cor_AF <- cor(df$Global_active_power,df$Sub_metering_2,method="pearson")
cor_AG <- cor(df$Global_active_power,df$Sub_metering_3,method="pearson")

cor_BC <- cor(df$Global_reactive_power,df$Voltage,method="pearson")
cor_BD <- cor(df$Global_reactive_power,df$Global_intensity,method="pearson")
cor_BE <- cor(df$Global_reactive_power,df$Sub_metering_1,method="pearson")
cor_BF <- cor(df$Global_reactive_power,df$Sub_metering_2,method="pearson")
cor_BG <- cor(df$Global_reactive_power,df$Sub_metering_3,method="pearson")

cor_CD <- cor(df$Voltage,df$Global_intensity,method="pearson")
cor_CE <- cor(df$Voltage,df$Sub_metering_1,method="pearson")
cor_CF <- cor(df$Voltage,df$Sub_metering_2,method="pearson")
cor_CG <- cor(df$Voltage,df$Sub_metering_3,method="pearson")

cor_DE <- cor(df$Global_intensity,df$Sub_metering_1,method="pearson")
cor_DF <- cor(df$Global_intensity,df$Sub_metering_2,method="pearson")
cor_DG <- cor(df$Global_intensity,df$Sub_metering_3,method="pearson")

cor_EF <- cor(df$Sub_metering_1,df$Sub_metering_2,method="pearson")
cor_EG <- cor(df$Sub_metering_1,df$Sub_metering_3,method="pearson")

cor_FG <- cor(df$Sub_metering_2,df$Sub_metering_3,method="pearson")

correlation_matrix <- round(cor(df[ ,3:ncol(df)]),2)
ggcorrplot(correlation_matrix)

#partC
#choose 6AM to 9AM as the time window for day hours, and 9PM to 12AM as the time window for night hours.

#24hour graph
dfR_weekdays24_day_ave <- 0:1439
dfR_weekdays24_night_ave <- 0:1439
dfR_weekend24_day_ave <- 0:1439
dfR_weekend24_night_ave <- 0:1439

#function to put day average in a 24 hour gap
blackBoxDay <- function(myData, startHour){
  resultData <- 0:1439
  for(i in 0:359){
    resultData[i + 1] <- NA
  }
  for(i in 360:539){
    myData_minute <- subset(myData, (60 * (myData$Time$hour) + myData$Time$min) == i)
    avg <- mean(myData_minute$Global_intensity)
    resultData[i + 1] <- avg
  }
  for(i in 540:1439){
    resultData[i + 1] <- NA
  }
  return(resultData)
}

#function to put night average in a 24 hour gap
blackBoxNight <- function(myData, startHour){
  resultData <- 0:1439
  for(i in 0:1259){
    resultData[i + 1] <- NA
  }
  for(i in 1260:1439){
    myData_minute <- subset(myData, (60 * (myData$Time$hour) + myData$Time$min) == i)
    avg <- mean(myData_minute$Global_intensity)
    resultData[i + 1] <- avg
  }
  return(resultData)
}
#24hour
dfR_weekdays24_day_ave <- blackBoxDay(df_weekdays_day,  6)
dfR_weekdays24_night_ave <- blackBoxNight(df_weekdays_night,  21)
dfR_weekend24_day_ave <- blackBoxDay(df_weekend_day,  6)
dfR_weekend24_night_ave <- blackBoxNight(df_weekend_night,  21)
#----------------------------------------------------------------------------------------------------------
#24 hour

temp<-subset(df_weekdays, df_weekdays$Time$hour >= 0 & df_weekdays$Time$hour < 24)
time_axis <- subset(as_hms(temp$Time), temp$Date == as.POSIXlt("7/5/2007",format = "%d/%m/%Y"))
x1 <- data.frame(dfR_weekdays24_day_ave,time_axis)

temp<-subset(df_weekdays, df_weekdays$Time$hour >= 0 & df_weekdays$Time$hour < 24)
time_axis <- subset(as_hms(temp$Time), temp$Date == as.POSIXlt("7/5/2007",format = "%d/%m/%Y"))
x2 <- data.frame(dfR_weekdays24_night_ave,time_axis)

temp<-subset(df_weekend, df_weekend$Time$hour >= 0 & df_weekend$Time$hour < 24)
time_axis <- subset(as_hms(temp$Time), temp$Date == as.POSIXlt("12/5/2007",format = "%d/%m/%Y"))
x3 <- data.frame(dfR_weekend24_day_ave,time_axis)

temp<-subset(df_weekend, df_weekend$Time$hour >= 0 & df_weekend$Time$hour < 24)
time_axis <- subset(as_hms(temp$Time), temp$Date == as.POSIXlt("12/5/2007",format = "%d/%m/%Y"))
x4 <- data.frame(dfR_weekend24_night_ave,time_axis)
#poly
ggplot(data = x4, mapping=aes(x=time_axis, y=dfR_weekend24_night_ave)) +
  geom_smooth(method="lm", formula= y~poly(x,6), se=FALSE, color="green") +
  geom_smooth(method="lm", data = x3, mapping=aes(y=dfR_weekend24_day_ave), formula= y~poly(x,6), se=FALSE, color="green") +
  geom_smooth(method="lm", data = x2, mapping=aes(y=dfR_weekdays24_night_ave), formula= y~poly(x,6), se=FALSE, color="red") +
  geom_smooth(method="lm", data = x1, mapping=aes(y=dfR_weekdays24_day_ave), formula= y~poly(x,6), se=FALSE, color="red") +  
  ggtitle("Linear regression of Global_intensity between weekdays and weekend") +
  labs(y = "Global_intensity", x = "Time")
#linear
ggplot(data = x4, mapping=aes(x=time_axis, y=dfR_weekend24_night_ave)) +
  geom_smooth(method="lm", formula= y~x, se=FALSE, color="green") +
  geom_smooth(method="lm", data = x3, mapping=aes(y=dfR_weekend24_day_ave), formula= y~x, se=FALSE, color="green") +
  geom_smooth(method="lm", data = x2, mapping=aes(y=dfR_weekdays24_night_ave), formula= y~x, se=FALSE, color="red") +
  geom_smooth(method="lm", data = x1, mapping=aes(y=dfR_weekdays24_day_ave), formula= y~x, se=FALSE, color="red") +
  ggtitle("Linear regression of Global_intensity between weekdays and weekend") +
  labs(y = "Global_intensity", x = "Time")
