#Josh comment

## Load libraries
library(readr)
library(ggplot2)

## Set wd
setwd("C:/Users/Richard/Documents/508")

## load the data
corn <- read_csv("Corn_EnergyFlux.csv")

Try1 <- corn
## So our data.frame is "Try1" from now on.

#Firstly, lets use the simple plot function, this fuction is already in base R library so no need to download any library
#The basic function is plot(x, y), where x and y are numeric vectors denoting the (x,y) points to plot.

x=Try1$RN_1_1_1 # define x and y, you can select the column you want from the data.frame(Try1) using "$" notation
y=Try1$NEE

plot(x,y, type = "p") #just plot x vs. y with point scatter (type="l" is for line)

plot(x, y, main="Scatterplot Example") ## adding a title on the plot

plot(x, y, main="Scatterplot Example",
     xlab="NetRad ", ylab="NEE")             ## adding labels for x and y axis

plot(x, y, main="Scatterplot Example",
     xlab="NetRad ", ylab="NEE", pch=20, col="red") ## changing the size of the points and the colour


##Now it is your turn:)

#Please try to make a point scatter plot of Net Radiation (column named as "RN_1_1_1" in the data.frame)
#versus Sensible Heat (column named as "H" in the data.frame) with following ;

#Figure title ="Energy Fluxes"
#labels for x and y axes are as "H(W/m2)" and "Rn(W/m2)" repectiverly.
#point coluour is different then default, choose what ever you want
#point size should be 18.

##THANKS!

x <- Try1$H
y <- Try1$RN_1_1_1

plot(x, y, main="Energy Fluxes",
     xlab= bquote('H '~(W/m^2)),ylab = "", pch=18, col="orange", grid()) #this one
title(ylab= bquote('Rn ('~W/m^2~')'), mgp=c(2,1,0)) #the ylabel must be made here so it is on the page

