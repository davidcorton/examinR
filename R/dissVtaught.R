dissVtaught <- function(x, output.name = "scatter.jpg", title = "", dimensions = c(1000, 1000)) {
  # Define device and margins
  jpeg(output.name, dimensions[1], dimensions[2], quality = 100)
  par(mar = c(5, 4, 2, 1), oma = c(0, 0, 0, 0), mfrow = c(1, 1), pty = "s")

  # Exclude cases that don't have both taught marks (at least 2 modules) and a dissertation
  x <- x[!is.na(Mark) & nModules > 1]

  # Set markers
  colcodes <- c("darkgoldenrod", "darkblue", "grey")
  markers <- c(21, 24)

  # Plot scatter
  with(x, plot(TaughtAverage, Mark, xlim = c(40, 100), ylim = c(40, 100),
               pch = markers[SSP], bg = colcodes[Gender],
               xlab = "Average taught mark", ylab = "Dissertation mark",
               cex = 3.5, cex.axis = 2, lwd = 3, cex.lab = 2.5))
  abline(0, 1)
  legend("bottomright", legend = c("Female", "Male", "Other", "No SSP", "SSP"), pch = c(21, 21, 21, 21, 24),
         pt.bg = c("darkgoldenrod", "darkblue", "grey", "white", "white"), bty = "n", cex = 2.5)

  # Close device and save file
  dev.off()
}
