compareMods <- function(x, output.name = "boxplots.jpg", title = "Module marks", dimensions = c(1500, 1000), screen = F, ...) {

  # Remove missing values and zero marks
  x <- x[!is.na(Mark) & !Mark == 0]

  # Perform Kruskal-Wallis test and save results
  KW <- kruskal.test(Mark ~ Module, data = x)

  # Order modules by median value
  summ <- x[, j = list("n" = length(Student), "Median" = median(Mark)), by = "Shortname"]
  summ <- summ[order(-Median)]
  summ[, Mod := factor(summ$Shortname, levels = summ$Shortname)]
  x <- merge(x, summ[, list(Shortname, Mod)], by = "Shortname")

  # Define margins and output device
  if(screen == F) {
    jpeg(output.name, dimensions[1], dimensions[2], quality = 100)
    par(mar = c(17, 6, 4, 1), oma = c(0, 0, 0, 0), mfrow = c(1, 1))
  }

  # Set markers
  colcodes <- c("darkgoldenrod", "darkblue", "grey")
  markers <- c(21, 24)

  # Plot boxplots
  with(x, boxplot(Mark ~ Mod, las = 2, outline = F,
                  cex.main = 2.5, cex = 3.5, cex.axis = 2, col = "grey", lwd = 3, cex.lab = 3,
                  main = paste0(title, " (KW: p = ", round(KW$p.value, 3), ")"),
                  ylab = "", xlab = "", ylim = c(25, 90)))
  mtext("Module mark", side = 2, line = 4, cex = 3)

  # Add reference lines for common grade boundaries
  abline(h = c(50, 60, 70, 80), lty = 3, col = "darkred")

  # Plot individual student values
  with(x, beeswarm(Mark ~ Mod, pwbg = colcodes[Gender], lwd = 2, pwpch = markers[SSP], method = "hex",
                   cex = 2.7, add = T, pch = 20, spacing = 0.7))
  text(x = 1:nrow(summ), y = 26, labels = summ$n, col="blue", cex = 2.5)

  # Close output device
  if(screen == F) {
    dev.off()
  }
}
