gridHist <- function(x, output.name = "gridHistogram.jpg", title = "Mark", dimensions = c(1800, 1000), screen = F) {
  # Calculate Mann-Whitney test result for genders
  MWgender <- wilcox.test(Mark ~ Gender, data = x[Gender %in% c("M", "F")])
  MW.SSP <- wilcox.test(Mark ~ SSP, data = x)

  # Define device and margins
  if(screen == F) {
    jpeg(output.name, dimensions[1], dimensions[2], quality = 100)
  }
  par(mar = c(4, 5, 4, 1), oma = c(0, 0, 0, 0), mfrow = c(2, 2), pty = "m")

  # Plot histograms and add overall legends
  markHist(x[Gender == "F", ], main = paste0("Female (MW: p=", round(MWgender$p.value, 3), ")"), col = "darkgoldenrod",
           legendSize = 2, cex.main = 2.5, cex = 2.5, cex.axis = 2, lwd = 2, cex.lab = 2.5)
  mtext("Frequency", side = 2, line = 3, cex = 2)

  markHist(x[SSP == "N", ], main = paste0("No SSP (MW: p=", round(MW.SSP$p.value, 3), ")"), col = "darkgreen",
           legendSize = 2, cex.main = 2.5, cex = 2.5, cex.axis = 2, lwd = 2, cex.lab = 2.5)

  markHist(x[Gender == "M", ], main = "Male", col = "darkblue",
           legendSize = 2, cex.main = 2.5, cex = 2.5, cex.axis = 2, lwd = 2, cex.lab = 2.5)
  mtext(title, side = 1, line = 3, cex = 2)
  mtext("Frequency", side = 2, line = 3, cex = 2)

  markHist(x[SSP == "Y", ], main = "SSP", col = "gray",
           legendSize = 2, cex.main = 2.5, cex = 2.5, cex.axis = 2, lwd = 2, cex.lab = 2.5)
  mtext(title, side = 1, line = 3, cex = 2)

  # Close output device
  if(screen == F) {
    dev.off()
  }
}
