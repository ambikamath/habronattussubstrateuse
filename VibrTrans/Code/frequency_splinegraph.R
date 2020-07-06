# This code is for stick. Do some replacements for other substrates.

StickFre <- read.csv("SigPro_Stick.csv")
StickFre0 <- subset(StickFre, Distance..mm.==0)
StickFre5 <- subset(StickFre, Distance..mm.==5)
StickFre10 <- subset(StickFre, Distance..mm.==10)

stick <-smooth.spline(StickFre0$Frequency..Hz., StickFre0$Relative.dB, spar = .6)
plot(stick, main = "Stick", xlab = "", ylab = "Relative Intensity (dB)", 
     cex.main = 3, cex.lab = 2, cex.axis = 2, ylim=c(-15, 0), xlim=c(0, 2000), xaxs = "i", yaxs = "i", col = "white")

stick <-smooth.spline(StickFre0$Frequency..Hz., StickFre0$Relative.dB, spar = .6)
lines(stick, col = "black", lwd = 6)

stick <-smooth.spline(StickFre5$Frequency..Hz., StickFre5$Relative.dB, spar = .6)
lines(stick, col = "red", lwd = 6)

stick <-smooth.spline(StickFre10$Frequency..Hz., StickFre10$Relative.dB, spar = .6)
lines(stick, col = "blue", lwd = 6)