markHist <- function(x, legendSize = 1, ...) {
  # Tidy up data
  x <- x[!is.na(Mark) & !Mark == 0]

  # Calculate summary stats
  xmax <- paste("Max = ", round(max(x$Mark), 2))
  xmin <- paste("Min = ", round(min(x$Mark), 2))
  xmean <- paste("Mean = ", round(mean(x$Mark), 2))
  xmedian <- paste("Median = ", round(median(x$Mark), 2))
  xSD <- paste("sd = ", round(sd(x$Mark), 2))
  n <- paste("n = ", nrow(x))

  # Plot histogram and legend
  if(nrow(x) > 0) {
    hist(x$Mark, xaxp = c(0, 100, 10), xlim = c(0, 100), xlab = "", ylab = "", ...)
    legend("topleft", legend = c(xmax, xmin, xmean, xmedian, xSD, n), bty = "n", text.col = "blue", cex = legendSize)
  }
}
