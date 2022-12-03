#install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.csv(file = "Group_Assignment_2_Dataset.txt", header = TRUE)
#install.packages("ggcorrplot")
#install.packages("gglot2")
library(ggplot2)
library(ggcorrplot)
#install.packages("data.table")
library(data.table)
#install.packages("hms")
library(hms)
#install.packages("forecast")
library(forecast)
library(TTR)
df$Date <- as.POSIXlt(df$Date,format = "%d/%m/%Y")
df$Time <- as.POSIXlt(df$Time,format = "%H:%M:%S")
# we only choose full size weeks
df <- subset(df, df$Date >= as.POSIXlt("7/1/2008",format = "%d/%m/%Y") & df$Date <= as.POSIXlt("28/12/2008",format = "%d/%m/%Y"))
# Moving average
df$Global_intensity <- ma(df$Global_intensity, 7)

smoothed_week <- df
# omit all NA values
smoothed_week <- na.omit(smoothed_week)
# blackBox is to get the normal week data
blackBox <- function(myData){
  agg_mean <- list()
    for( j in 1:7){
      if(j==7){
        temp_wday = 0
      }else{
        temp_wday = j
      }
      temp <- subset(smoothed_week, smoothed_week$Date$wday == temp_wday)
      agg_mean[[j]] <- aggregate(temp$Global_intensity,by=list(60*temp$Time$hour+temp$Time$min),FUN=mean)
  }
  return(agg_mean)
}
avg_smoothed_week <- blackBox(smoothed_week)

# group the normal week data into a data.frame
avg_smoothed_week2 <- aggregate(smoothed_week$Global_intensity,by=list(smoothed_week$Time$min,smoothed_week$Time$hour,smoothed_week$Date$wday),FUN=mean)
avg_smoothed_week2 <- setNames(avg_smoothed_week2, c("min","hour","wday","Global_intensity"))
avg_smoothed_week2 <- na.omit(avg_smoothed_week2)

# blackBox2 is to get each weeks' score by calculating the square of the standard deviation, I choose to use the square of the standard deviation
#as the score because (1) it can enlarge the gap between an anomaly week and the normal week so any anomaly week will be more easy to detect
#(2) it can deal with negative gap and positive gap by squaring them
blackBox2 <- function(myData, avg_smoothed_week){
  my_list <- list()
  max <- 0
  min <- +Inf
  max_week <- 0
  min_week <- 0

  for(p in 2:52){
    dat <- subset(myData, week(myData$Date) == p) 
    temp3 <- 0
    for(i in 0:6){
      for(j in 0:23){
        for(z in 0:59){
          temp <- subset(dat$Global_intensity, dat$Date$wday == i & dat$Time$hour == j & dat$Time$min == z)
          if (length(temp) == 0){
            temp <- 0
          }
          temp2 <- subset(avg_smoothed_week$Global_intensity, avg_smoothed_week$wday == i & avg_smoothed_week$hour == j & avg_smoothed_week$min == z)
          temp3 <- temp3 + (temp - temp2)^2/nrow(dat)
          my_list[p] <- temp3
        }
      }
    }
    if( temp3 > max){
      max <- temp3
      max_week <- p
    }
    if( temp3 < min){
      min <- temp3
      min_week <- p
    }
  }
  my_list[53] <- max_week
  my_list[54] <- min_week
  return(my_list)
}
weekList2 <- blackBox2(smoothed_week,avg_smoothed_week2)

# represent all anomaly score in the table anomalyScore
anomalyScore <- data.frame(matrix(nrow = 51,ncol =2))
anomalyScore <- setNames(anomalyScore, c("week","score"))
for(i in 2:52){
  anomalyScore$week[i-1] <- i
  anomalyScore$score[i-1] <- weekList2[i]
}
print(anomalyScore)

# prepare the normal week for the plot
normal_week <- list()
z<-0
for(i in 1:7){
  for(j in 1:1440){
    normal_week[z] <- avg_smoothed_week[[i]][j,2]
    z<-z+1
  }
}
normal_week <-data.frame(matrix(unlist(normal_week), nrow=length(1), byrow=TRUE))
normal_week <- t(normal_week)
normal_week <- setNames(data.frame(normal_week), "Global_intensity")

# prepare max_week and min_week for plot
max_week <- subset(smoothed_week, week(smoothed_week$Date) == weekList2[53])
min_week <- subset(smoothed_week, week(smoothed_week$Date) == weekList2[54])

# plot. Green line is the most anomaly week, red line is the least anomaly week and the black line is the normal week
ggplot(data = max_week, mapping=aes(x=1:nrow(max_week), y=Global_intensity)) +
  geom_smooth(method="lm", formula= y~poly(x,7), se=FALSE, color="green") + 
  geom_smooth(method="lm", data = min_week, mapping=aes(x=1:nrow(min_week), y=Global_intensity), formula= y~poly(x,7), se=FALSE, color="red") +
  geom_smooth(method="lm", data = normal_week, mapping=aes(x=1:nrow(normal_week), y=Global_intensity), formula= y~poly(x,7), se=FALSE, color="black")




