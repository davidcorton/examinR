studentSummary <- function(x) {
  # Make table of average taught module marks per student
  moduleMeans <- x[!Module == "Dissertation", j = list(TaughtAverage = mean(Mark), nModules = length(Module)), by = c("Student", "Gender", "SSP")]

  # Merge in the dissertation marks, keeping all cases
  moduleMeans <- merge(x[Module == "Dissertation", list(Student, Mark, Programme, Gender, SSP)], moduleMeans, by = c("Student", "Gender", "SSP"), all = T)

  # Calculate overall mark where there are four modules and a dissertation available
  moduleMeans[nModules == 4 & !is.na(Mark), OverallMark := (Mark + TaughtAverage) / 2]

  # Return summary table
  moduleMeans
}
