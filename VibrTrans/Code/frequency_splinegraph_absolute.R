filter <- read.csv("stick_abs.csv")
filter0 <- subset(filter, Distance..mm.==0)
filter5 <- subset(filter, Distance..mm.==5)
filter10 <- subset(filter, Distance..mm.==10)

abs <-smooth.spline(filter0$Frequency..Hz., filter0$fig, spar = .6)
plot(abs, main = "Stick", xlab = "", ylab = "Magnitude (10^-6 m/s)", 
     cex.main = 3, cex.lab = 2, cex.axis = 2, ylim=c(0, 8), xlim=c(0, 2000), xaxs = "i", yaxs = "i", col = "white")

abs <-smooth.spline(filter0$Frequency..Hz., filter0$fig, spar = .6)
lines(abs, col = "black", lwd = 6)

abs <-smooth.spline(filter5$Frequency..Hz., filter5$fig, spar = .6)
lines(abs, col = "red", lwd = 6)

abs <-smooth.spline(filter10$Frequency..Hz., filter10$fig, spar = .6)
lines(abs, col = "blue", lwd = 6)