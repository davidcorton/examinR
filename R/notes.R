# packages to install when writing code manually
library(data.table)
library(googlesheets4)
library(lawstat)
library(beeswarm)

# Workflow is as follows:

# 1.Read in data
# 2.Tidy up column names etc - should be avoidable with templates!
# 3. Analysis, which depends on the situation.

# a) plot histogram with statistics
# b) plot histos below each other
# c) plot histo comparison grid
# d) boxplot comparing modules
# e) dissertation vs. taught course mark

# Let's start with a and d, as the most common...
# We'll work from this year's data for now

data <- data.table(read_sheet("https://docs.google.com/spreadsheets/d/1gkBcHFYMq3uyyrS_IYb17X7-RCLllLNa7zYVTncxAFs/edit#gid=0"))
data[, Module := as.factor(Module)]
data[, Shortname := as.factor(Shortname)]
data[, Code := as.factor(Code)]
data[, Term := as.factor(Term)]
data[, Student := as.factor(Student)]
data[, Gender := as.factor(Gender)]
data[is.na(SSP), SSP := "N"]
data[, SSP := as.factor(SSP)]
data[, Programme := as.factor(Programme)]
data[, Mark := as.numeric(Mark)]


x <- data[Term == "T1", ]
output.name <- "output.jpg"
title <- "Module marks"
dimensions = c(1600, 1000)
colcodes <- c("darkgoldenrod", "darkblue", "grey")
markers <- c(21, 24)

compareMods <- function(x, output.name = "boxplots.jpg", title = "Module marks", dimensions = c(1600, 1000), ...) {

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
  jpeg(output.name, dimensions[1], dimensions[2], quality = 100)
  par(mar = c(17, 6, 4, 1), oma = c(0, 0, 0, 0), mfrow = c(1, 1))

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
                        cex = 3, add = T, pch = 20, spacing = 0.8))
  text(x = 1:nrow(summ), y = 26, labels = summ$n, col="blue", cex = 2.5)

  # Close output device
  dev.off()

}

##  Now the basic function for histograms, which will be called by wrapper functions for different formats

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
  hist(x$Mark, xaxp = c(0, 100, 10), xlim = c(0, 100), xlab = "", ylab = "", ...)
  legend("topleft", legend = c(xmax, xmin, xmean, xmedian, xSD, n), bty = "n", text.col = "blue", cex = legendSize)
}

## Wrapper for a single module
dimensions = c(1000, 1000)

moduleHist <- function(x, output.name = "histogram.jpg", title = "", dimensions = c(1000, 1000)) {
  # Define device and margins
  jpeg(output.name, dimensions[1], dimensions[2], quality = 100)
  par(mar = c(17, 6, 4, 1), oma = c(0, 0, 0, 0), mfrow = c(1, 1))

  # Plot histogram and axis labels
  markHist(x, main = title, col = "grey", legendSize = 3,
           cex.main = 2.5, cex = 3.5, cex.axis = 2.5, lwd = 3, cex.lab = 3)
  mtext("Module mark", side = 1, line = 4, cex = 3)
  mtext("Frequency", side = 2, line = 4, cex = 3)

  # Close device and save file
  dev.off()
}

## Wrapper for grid of histograms by gender and SSP status
gridHist <- function(x, output.name = "gridHistogram.jpg", title = "Mark", dimensions = c(1600, 1000)) {
  # Define device and margins
  jpeg(output.name, dimensions[1], dimensions[2], quality = 100)
  par(mar = c(4, 5, 4, 1), oma = c(0, 0, 0, 0), mfrow = c(2, 2))

  # Define groups
  genders <- c("F", "M", "F", "M")
  SSPs <- c("N", "N", "Y", "Y")
  titles <- c("Female, no SSP", "Male, no SSP", "Female, SSP", "Male, SSP")
  cols <- c("darkgoldenrod", "darkblue", "darkred", "darkgreen")
  ylabs <- c("Frequency", "", "Frequency", "")
  xlabs <- c("", "", title, title)

  # Plot histograms and add overall legends
  for(i in 1:4) {
    markHist(x[Gender == genders[i] & SSP == SSPs[i], ], main = titles[i], col = cols[i],
             legendSize = 2, cex.main = 2.5, cex = 2.5, cex.axis = 2, lwd = 2, cex.lab = 2.5)
    mtext(xlabs[i], side = 1, line = 3, cex = 2)
    mtext(ylabs[i], side = 2, line = 3, cex = 2)
  }

  # Close output device
  dev.off()

}

gridHist(x)
