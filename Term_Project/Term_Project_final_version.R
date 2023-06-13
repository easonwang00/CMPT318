#install.packages("rstudioapi")
library(rstudioapi)
#install.packages("ggcorrplot")
#install.packages("gglot2")
library(ggplot2)
library(ggcorrplot)
#install.packages("data.table")
library(data.table)
#install.packages("hms")
library(hms)
#install.packages("dplyr")
library(dplyr)
#install.packages("depmixS4")
library(depmixS4)
#install.packages("devtools")
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
#install.packages("mice")
library(mice)
#install.packages("missForest")
library(missForest)
#install.packages("dplyr")
library(dplyr) 


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.csv(file = "TermProjectData.txt", header = TRUE)

# PMM (Predictive Mean Matching) For numeric variables
# we use mice() to fill the na values
dfM <- mice(df, m=1, meth='pmm', seed=500)
summary(dfM)
df <- complete((dfM))
df$Date <- as.POSIXlt(df$Date,format = "%d/%m/%Y")
df$Time <- as.POSIXlt(df$Time,format = "%H:%M:%S")
# To scale the raw data using standardization prior to applying PCA.
df$Global_active_power <- scale(df$Global_active_power, center = TRUE, scale = TRUE)
df$Global_reactive_power <- scale(df$Global_reactive_power, center = TRUE, scale = TRUE)
df$Voltage <- scale(df$Voltage, center = TRUE, scale = TRUE)
df$Global_intensity <- scale(df$Global_intensity, center = TRUE, scale = TRUE)
df$Sub_metering_1 <- scale(df$Sub_metering_1, center = TRUE, scale = TRUE)
df$Sub_metering_2 <- scale(df$Sub_metering_2, center = TRUE, scale = TRUE)
df$Sub_metering_3 <- scale(df$Sub_metering_3, center = TRUE, scale = TRUE)

# To prepare for PCA
df2 <- subset(df, select = -c(Date,Time) )
# df2 is for feature selection
pcaM <- prcomp(df2)
print(pcaM$rotation)
summary(pcaM)
ggbiplot(pcaM, alpha = 0)

# We choose Global_active_power Global_intensity because those two features have the top 2 highest 
# absolutely value in PC1 which is the most important principle component 
#--------------------------------------------------------------------------------------------------------------------------------
# We choose 18-20PM Monday as our time window
t1 <- 18
t2 <- 20
featureList <- list(Global_active_power~1, Global_intensity~1)
familyList <- list(gaussian(), gaussian())
# df8 is our traning set
df8 <- subset(df, df$Time$hour >= t1 & df$Time$hour < t2)
df8 <- subset(df8, df8$Date$wday == 1)
df8 <- subset(df8, df8$Date <= as.POSIXlt("08/06/2009",format = "%d/%m/%Y"))
li <- rep(120, nrow(df8)/120)
set.seed(1)
mod1 <- depmix(response = featureList, data = df8, nstates = 4, ntimes = li, family = familyList)
fm1 <- fit(mod1)
mod2 <- depmix(response = featureList, data = df8, nstates = 8, ntimes = li, family = familyList)
fm2 <- fit(mod2)
mod3 <- depmix(response = featureList, data = df8, nstates = 10, ntimes = li, family = familyList)
fm3 <- fit(mod3)
mod4 <- depmix(response = featureList, data = df8, nstates = 14, ntimes = li, family = familyList)
fm4 <- fit(mod4)
mod5 <- depmix(response = featureList, data = df8, nstates = 15, ntimes = li, family = familyList)
fm5 <- fit(mod5)
mod6 <- depmix(response = featureList, data = df8, nstates = 16, ntimes = li, family = familyList)
fm6 <- fit(mod6)
mod7 <- depmix(response = featureList, data = df8, nstates = 17, ntimes = li, family = familyList)
fm7 <- fit(mod7)
# plot the log-likelihood and bic for different states
value = c(logLik(fm1),logLik(fm2),logLik(fm3),logLik(fm4),logLik(fm5),logLik(fm6),logLik(fm7))
bic = c(BIC(fm1),BIC(fm2),BIC(fm3),BIC(fm4),BIC(fm5),BIC(fm6),BIC(fm7))
dfplot <- data.frame(state = c(4,8,10,14,15,16,17), logLike, bic )
# red is log, blue is bic
ggplot(dfplot, aes(state)) + geom_line(aes(y = value), color = "red") + geom_line(aes(y = bic), color = "blue") 

# we choose state = 15 which is fm5
# the model after training
fm <- fm5
# dftest is our traning set
dftest <- subset(df, df$Time$hour >= t1 & df$Time$hour < t2)
dftest <- subset(dftest, dftest$Date$wday == 1)
dftest <- subset(dftest, dftest$Date > as.POSIXlt("08/06/2009",format = "%d/%m/%Y"))
li <- rep(120, nrow(dftest)/120)
modTest1 <- depmix(response = featureList, data = dftest, nstates = 15, ntimes = li, family = familyList)
modTest1 <- setpars(modTest1,getpars(fm))
#find loglikelihood of the test model
fb <- forwardbackward(modTest1)
fb$logLike

#compare normalize log-likelihood of train and test
dfcompare <- data.frame(models = c("train","test") , logLike = c(logLik(fm)/130,fb$logLike/25))
dfcompare

#anomaly testing
dfanomaly1 <- read.csv(file = "DataWithAnomalies1.txt", header = TRUE)
dfanomaly1$Date <- as.POSIXlt(dfanomaly1$Date,format = "%d/%m/%Y")
dfanomaly1$Time <- as.POSIXlt(dfanomaly1$Time,format = "%H:%M:%S")
dfanomaly1 <- subset(dfanomaly1, dfanomaly1$Time$hour >= t1 & dfanomaly1$Time$hour < t2)
dfanomaly1 <- subset(dfanomaly1, dfanomaly1$Date$wday == 1)
li <- rep(120, nrow(dfanomaly1)/120)

modAnomaly1 <- depmix(response = featureList, data = dfanomaly1, nstates = 15, ntimes = li, family = familyList)
modAnomaly1 <- setpars(modAnomaly1,getpars(fm))
fb1 <- forwardbackward(modAnomaly1)
fb1$logLike

dfanomaly2 <- read.csv(file = "DataWithAnomalies2.txt", header = TRUE)
dfanomaly2$Date <- as.POSIXlt(dfanomaly2$Date,format = "%d/%m/%Y")
dfanomaly2$Time <- as.POSIXlt(dfanomaly2$Time,format = "%H:%M:%S")
dfanomaly2 <- subset(dfanomaly2, dfanomaly2$Time$hour >= t1 & dfanomaly2$Time$hour < t2)
dfanomaly2 <- subset(dfanomaly2, dfanomaly2$Date$wday == 1)
li <- rep(120, nrow(dfanomaly2)/120)

modAnomaly2 <- depmix(response = featureList, data = dfanomaly2, nstates = 15, ntimes = li, family = familyList)
modAnomaly2 <- setpars(modAnomaly2,getpars(fm))
fb2 <- forwardbackward(modAnomaly2)
fb2$logLike


dfanomaly3 <- read.csv(file = "DataWithAnomalies3.txt", header = TRUE)
dfanomaly3$Date <- as.POSIXlt(dfanomaly3$Date,format = "%d/%m/%Y")
dfanomaly3$Time <- as.POSIXlt(dfanomaly3$Time,format = "%H:%M:%S")
dfanomaly3 <- subset(dfanomaly3, dfanomaly3$Time$hour >= t1 & dfanomaly3$Time$hour < t2)
dfanomaly3 <- subset(dfanomaly3, dfanomaly3$Date$wday == 1)
li <- rep(120, nrow(dfanomaly3)/120)

modAnomaly3 <- depmix(response = featureList, data = dfanomaly3, nstates = 15, ntimes = li, family = familyList)
modAnomaly3 <- setpars(modAnomaly3,getpars(fm))
fb3 <- forwardbackward(modAnomaly3)
fb3$logLike

#compare log-likelihood of all
logLike<- c(logLik(fm)/130, fb$logLike/25 ,fb1$logLike/(nrow(dfanomaly1)/120),fb2$logLike/(nrow(dfanomaly2)/120),fb3$logLike/(nrow(dfanomaly3)/120))
dfcompare <- data.frame(models = c("train","test","anomaly1","anomaly2","anomaly3") , logLike)
dfcompare

# check the mean of dfanomaly3
mean(dfanomaly3$Global_active_power)
mean(df8$Global_active_power)
# scale the dfanomaly3 to see if now our model can apply on it
dfanomaly3 <- read.csv(file = "DataWithAnomalies3.txt", header = TRUE)
dfanomaly3$Date <- as.POSIXlt(dfanomaly3$Date,format = "%d/%m/%Y")
dfanomaly3$Time <- as.POSIXlt(dfanomaly3$Time,format = "%H:%M:%S")
dfanomaly3 <- subset(dfanomaly3, dfanomaly3$Time$hour >= t1 & dfanomaly3$Time$hour < t2)
dfanomaly3 <- subset(dfanomaly3, dfanomaly3$Date$wday == 1)
dfanomaly3$Global_active_power <- scale(dfanomaly3$Global_active_power, center = TRUE, scale = TRUE)
dfanomaly3$Global_reactive_power <- scale(dfanomaly3$Global_reactive_power, center = TRUE, scale = TRUE)
dfanomaly3$Voltage <- scale(dfanomaly3$Voltage, center = TRUE, scale = TRUE)
dfanomaly3$Global_intensity <- scale(dfanomaly3$Global_intensity, center = TRUE, scale = TRUE)
dfanomaly3$Sub_metering_1 <- scale(dfanomaly3$Sub_metering_1, center = TRUE, scale = TRUE)
dfanomaly3$Sub_metering_2 <- scale(dfanomaly3$Sub_metering_2, center = TRUE, scale = TRUE)
dfanomaly3$Sub_metering_3 <- scale(dfanomaly3$Sub_metering_3, center = TRUE, scale = TRUE)
li <- rep(120, nrow(dfanomaly3)/120)

modAnomaly3 <- depmix(response = featureList, data = dfanomaly3, nstates = 15, ntimes = li, family = familyList)
modAnomaly3 <- setpars(modAnomaly3,getpars(fm))
fb3 <- forwardbackward(modAnomaly3)
fb3$logLike

#compare log-likelihood of all
logLike<- c(logLik(fm)/130, fb$logLike/25 ,fb1$logLike/(nrow(dfanomaly1)/120),fb2$logLike/(nrow(dfanomaly2)/120),fb3$logLike/(nrow(dfanomaly3)/120))
dfcompare <- data.frame(models = c("train","test","anomaly1","anomaly2","anomaly3") , logLike)
dfcompare


#----------------------------------------------------------------------------------------------------------------------