dev.off()
#Planck's law describes the spectral density of electromagnetic radiation emitted by a black body in thermal equilibrium at a given temperature T

h<-6.626*10^-34 # Planck's constant (J*s)
c<-2.997925*10^8 # speed of light (m/s)  or 10^17 nm/s
k<-1.381*10^-23 # Boltzmann's constant (J/K)
T2 = 6000
lambda2 = seq(1.5e-7,350e-7, by=1e-9)
lambda3 = seq(1.5e-7,350e-7, by=1e-9)
y2 = (h*c)/(lambda2*k*T2)
I2=((2*pi)*(h)*(c**2)/((lambda2**5)*(exp(y2)-1)))

T3 = 300
y3 = (h*c)/(lambda3*k*T3)
I3=((2*pi)*(h)*(c**2)/((lambda3**5)*(exp(y3)-1)))

plot(lambda2,I2,log="x", ylim=c(1e7,1e14),xlab='Wavelength (m)',ylab='Spectral Emittance (W/m^2/m)',main='Spectral emmittance for visible range')
tot2 = sum(I2)*diff(lambda2)[1]
wein2 = lambda2[which.max(I2)]
tot3 = sum(I3)*diff(lambda3)[1]
wein3 = lambda3[which.max(I3)]
cat("Observed value of total power: @",T2, "K =",tot2*1e-6, "MW/m^2", "peak wavelength:", wein2*1e9,"nm" )
cat("Observed value of total power: @",T3, "K =",tot3*1e-6, "MW/m^2", "peak wavelength:", wein3*1e9,"nm" )

par(new = TRUE)
plot(lambda2,I3,log="x", ylim=c(1e6,1e8),xaxt = "n", yaxt ="n", ylab = "", xlab = "")

points(lambda2,I3, col="red",pch="*")
lines(lambda2,I3, col="red",lty=2)
abline(v=lambda2[which.max(I2)], col="black")
abline(v=lambda2[which.max(I3)], col="red")

