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

