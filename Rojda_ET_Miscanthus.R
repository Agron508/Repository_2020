##Feb 25, 2020
##Rojda
##lets try to use "Evapotranspiration" package for our Miscanthus data set from 2009-2011.

##we can arrange the "data" and "constant" files by using the previous rscript (Rojda_ET_example) 


library(Evapotranspiration)

#Import the DailyET_Miscanthus_2009-2011.csv file

DailyET_Miscanthus_2009.2011 <- read.csv("C:/Users/myAdmins/Desktop/Andy Course/DailyET_Miscanthus_2009-2011.csv")

ET<-DailyET_Miscanthus_2009.2011 ##rename the file to keep the original one



#download the pachage example again to see the "data" and " constant" files

data("processeddata")
data("constants")

##you can find defination of the all parameters in constants and data in the "Packages" segment on the left bottom window. 
##Go and click "Packages" and find "Evapotranspiration", and Under "Evapotranspiration" package find "constants" and "data"

#constants
#Elev - ground elevation above mean sea level in m, !!224m(for our case)
#lambda - latent heat of vaporisation = 2.45 MJ.kg^-1,
#lat_rad - latitude in radians,
#Gsc - solar constant = 0.0820 MJ.m^-2.min^-1,
#z - height of wind instrument in m,
#sigma - Stefan-Boltzmann constant = 4.903*10^-9 MJ.K^-4.m^-2.day^-1.
#G - soil heat flux in MJ.m^-2.day^-1, = 0 when using daily time step.



##So we should rearrange the "data" list with our variables and also rearrange "constants" with our values.



library(argosfilter) ##I used this to convert latitute in radians since constants wanted in rad (see above)


##Our site constants

##and then changed the parameters with our site values

##Elevation= 224 m
##latitude =40.062818
##wind anemometer instrument height,z  = 4 m
##annuam mean precipitation, PA= 504 mm


radian(40.062818)#argosfilter package
[1] 0.6992281

constants1<-constants##to keep original example"constants" as " constants1" to check our changes.


constants$Elev=224
constants$lat_rad=0.6992281
constants$z=4 #wind isntrument height, m
constants$PA=504 #annual mean precipitation mm



##please try to change variables in the example "data"  with "ET" , example;

data1<-data ##to keep original example"data" as " data1" to check our changes.

data$Date.daily<-ET$Date.daily

##please continue to changes the variables like this using our "ET" data sheet to create appropriate "data" list for the pakage use.


##Thanks!

