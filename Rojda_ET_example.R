##17 February 2020
##Rojda
##"Evapotranspiration" package usage exercise


##First install the "Evapotranspiration" package

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

#Josh's Plot
totalET = cumsum(results$ET.Monthly)
plot(results$ET.Monthly, col = "aquamarine4", main = "Monthly ET", xlab = "Year", ylab = "ET (mm)", lwd = "4")
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
