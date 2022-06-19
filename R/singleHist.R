singleHist <- function(x, output.name = "histogram.jpg", title = "", dimensions = c(1000, 1000), screen = F) {
  # Define device
  if(screen == F) {
    jpeg(output.name, dimensions[1], dimensions[2], quality = 100)
  }

  # Define margins
  par(mar = c(17, 6, 4, 1), oma = c(0, 0, 0, 0), mfrow = c(1, 1))

  # Plot histogram and axis labels
  markHist(x, main = title, col = "grey", legendSize = 3,
           cex.main = 2.5, cex = 3.5, cex.axis = 2.5, lwd = 3, cex.lab = 3)
  mtext("Module mark", side = 1, line = 4, cex = 3)
  mtext("Frequency", side = 2, line = 4, cex = 3)

  # Close device and save file
  if(screen == F) {
    dev.off()
  }
}
