##17 February 2020
##Rojda
##"Evapotranspiration" package usage exercise


##First install the "Evapotranspiration" package
library(tidyverse)
library(Evapotranspiration) ##  Hit the "Run" and go to the right bottom window and click the "Packages" segment to check if "Evapotranspiration"  is there or not. 
##If it did not work somehow, please try the "install" segment just below the "packages" segment. Click "install" and type "Evapotranspiration" in the Packages. 
##Now please click the "Evapotranspiration" in the Packages segment on the left bottom window. And find the "ET.PenmanMonteith". 
##Click "ET.PenmanMonteith" and please read the document. At the end of the text, there is an example of data that you can run.

##Example

data("processeddata")
data("constants")

results <- ET.PenmanMonteith(data, constants, ts="daily", solar="sunshine hours", 
                             wind="yes", crop = "short", message="yes", AdditionalStats="yes", save.csv="yes") 



##try to make simple plot

plot(results$ET.Daily)

##Your Turn:
##Now please make some changes to the graph. You can use the previous R script in the Announcement in Canvas.

#<<<<<<< HEAD
#Plot by Richard
plot(data$Date.daily,results$ET.Daily, main="Penman-Monteith Formulation for Kent Town, Adelaide",
     xlab="Year", ylab="Evapotransporation (mm)")
grid()
#=======
#Josh's Plot ====================================================================
totalET = cumsum(results$ET.Monthly) #Cumulative ET
par(mar=c(4, 4, 4, 4)) #Set the margine size around the plot

#Plot Monthly ET, format axis
plot(results$ET.Monthly, ylim = c(0,250), col = "deepskyblue", main = "Evapotranspiration", xlab = "Year", ylab = "Monthly ET (mm)", lwd = "3")
axis(2, ylim=c(0,5000), col="deepskyblue",col.axis="deepskyblue")

#Allow for a second line on the plot
par(new=TRUE)

#Plot Cumulative ET, format axis
plot(totalET, ylim = c(0,5000), col = "chartreuse4", axes = FALSE, xlab="", ylab = "", lwd = "3")
mtext("Cumulative ET (mm)",side=4,col="black", line=3)
axis(4, ylim=c(0,5000), col="chartreuse4",col.axis="chartreuse4")

#Set gridlines on plot
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

#Andy's Plot ====================================================================
tmax=data$Tmax
tdew=data$Tdew
tmin=data$Tmin
plot(tmax,results$ET.Daily, xlab="Maximum Temperature (C)",ylab = "Daily ET (mm)", lwd = "3")
plot(tmin,results$ET.Daily, xlab="Minmum Temperature (C)",ylab = "Daily ET (mm)", lwd = "3")
plot(tdew,results$ET.Daily, xlab="Dew Point Temperature (C)",ylab = "Daily ET (mm)", lwd = "3")

plot(tmin,tdew, xlab="Dew Point Temperature (C)",ylab = "Minmum Temperature (C)", lwd = "3")
#>>>>>>>Tyler's plot

tmax=data$Tmax
tdew=data$Tdew
tmin=data$Tmin
plot(tmax,tmin)
###This is where we got stuck.  Need to create a monthly variable to average by.
maxday<-aggregate(cbind(data$Tmax)~data$Date.monthly,data,max)
plot(data$Date.monthly)
plot(tmax,results$ET.Monthly, xlab="Maximum Temperature (C)",ylab = "Monthly ET (mm)", lwd = "3")


###Rojda`s response for Tyler`s Plot===============================================================

monthTmax<-data.frame(data$Tmax)
monthTmax$Date<-data$Date.daily
colnames(monthTmax)<-c("Tmax","DateTime")
class(monthTmax$DateTime)
[1] "Date"

monthTmax$Month<-as.Date(cut(monthTmax$DateTime,breaks = "month"))
maxday<-aggregate(cbind(monthTmax$Tmax)~monthTmax$Month,monthTmax,max)

colnames(maxday)<-c("Month","Tmax")

library(lubridate)
#--make the dataframe into a tibble bc they are easier to view
maxday <- as_tibble(maxday) %>% 
  mutate(ET_monthly = as.vector(results$ET.Monthly),
         year_month = Month,
         year_id = year(Month))

maxday %>% write_csv("data_ET-for-shiny.csv")

maxday %>% 
  ggplot(aes(Tmax, ET_monthly)) + 
  geom_point(aes(color = as.factor(year_id)), size = 5)

plot(maxday$Tmax,results$ET.Monthly, xlab="Maximum Temperature (C)",ylab = "Monthly ET (mm)", lwd = "3", col="red") #this plot shows monthly cumulative ET versus maximum temperature @ Month


