multiHist <- function(data, moduleList, output.name = "histogram.jpg",
                      title = "", dimensions = c(1500, 700), textSize = 2,
                      screen = F) {
  # Define device
  if(screen == F) {
    jpeg(output.name, dimensions[1], dimensions[2], quality = 100)
  }

  # Set up plot panes
  par(mfrow = c( length(moduleList), 1 ), mar = c(3, 4, 2, 1), oma = c(3, 0, 0, 0))

  # Run through module list and plot histogram for each
  for(i in 1:length(moduleList)) {
    markHist(data[Shortname == moduleList[i], ], main = moduleList[i], breaks = 10,
             legendSize = textSize, cex.axis = textSize, cex.lab = textSize, cex.main = textSize)
  }

  # Add x-axis label
  mtext("Module mark", side = 1, line = 1, outer = T, cex = textSize * 0.8)

  # Close device
  if(screen == F) {
    dev.off()
  }
}
