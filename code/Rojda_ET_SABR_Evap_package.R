
#March 2, 2020

##appliying "Evapotranspiratio" package for the SABR miscanthus site

#I prepared the available format data set for the package and uploaded in the Box under Agron 508-S20 folder named as "DailyET_SABR_miscanthus"
#please first import "DailyET_SABR_miscanthus" csv file.



DailyET_SABR_miscanthus <- read.csv("C:/Users/myAdmins/Box/AGRON 508-S20/R Study/DailyET_SABR_miscanthus.csv")
DailyET1<-DailyET_SABR_miscanthus

library(Evapotranspiration)
#download example data
data("processeddata")
data("constants")

#constants
#Elev - ground elevation above mean sea level in m, !!224m(for our case)
#lambda - latent heat of vaporisation = 2.45 MJ.kg^-1,
#lat_rad - latitude in radians,
#Gsc - solar constant = 0.0820 MJ.m^-2.min^-1,
#z - height of wind instrument in m,
#sigma - Stefan-Boltzmann constant = 4.903*10^-9 MJ.K^-4.m^-2.day^-1.
#G - soil heat flux in MJ.m^-2.day^-1, = 0 when using daily time step.

library(argosfilter)
radian( 41.9985)#argosfilter package
[1] 0.7330121


#and then changed the parameters with our site values

data1<-data
constants1<-constants
constants$Elev=316.9
constants$lat_rad=0.7330121
constants$z=4 #wind isntrument height, m
constants$PA=503 #it is for  the time from june 2019-feb 2020 precipitation mm


# change the parameters values in example"data" list with our site values.

data$Date.daily<-DailyET1$Date.daily
data$Date.monthly<-DailyET1$Date.monthly
data$J<-DailyET1$J
data$i<-DailyET1$i
data$Ndays<-DailyET1$ndays
data$Tmax<-DailyET1$Tmax
data$Tmin<-DailyET1$Tmin
data$Tdew<-DailyET1$Tdew
data$RHmax<-DailyET1$RHmax
data$RHmin<-DailyET1$RHmin
data$uz<-DailyET1$uz
data$n<-DailyET1$n



# changing class of the variables in the "data" list

library(zoo)

data$Date.daily<-ymd(data$Date.daily)
data$J<-zoo(yday(data$Date.daily))
data$Ndays<-zoo(as.double(data$Ndays))
data$Tmax<-zoo(data$Tmax)
data$Tmin<-zoo(data$Tmin)
data$Tdew<-zoo(data$Tdew)
data$RHmax<-zoo(data$RHmax)
data$RHmin<-zoo(data$RHmin)
data$uz<-zoo(data$uz)
data$n<-zoo(data$n)

results1 <- ET.PenmanMonteith(data, constants, ts="daily", solar="sunshine hours", 
                              wind="yes", crop = "short", message="yes", AdditionalStats="yes", save.csv="yes")


#to compare with the ET data from Ec:


plot(DailyET1$Date,DailyET1$ET_EC)

plot.zoo( cbind(results1$ET.Daily, DailyET1$ET_EC),
          plot.type = "single", 
          col = c("red", "blue"))

#you can try to plot the figure using ggplot as we used before


