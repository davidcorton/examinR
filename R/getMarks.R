getMarks <- function(marks, students, modules) {

  # Read in marks, plus look-up tables for students and modules
  marks <- data.table(read_sheet(marks))
  students <- data.table(read_sheet(students))
  modules <- data.table(read_sheet(modules))

  # Merge the three tables together
  data <- merge(marks, students, by = "Student")
  data <- merge(data, modules, by = "Code")

  # Make sure all the variables are the right types
  data[, Module := as.factor(Module)]
  data[, Shortname := as.factor(Shortname)]
  data[, Code := as.factor(Code)]
  data[, Term := as.factor(Term)]
  data[, Student := as.factor(Student)]
  data[, Gender := as.factor(Gender)]
  data[, Programme := as.factor(Programme)]

  # Specify SSP categories
  data[is.na(SSP), SSP := "N"]
  data[, SSP := as.factor(SSP)]

  # If text has found its way into Mark (most likely as a note after the actual numbers), purge it
  if(!class(data$Mark) == "numeric") {
    data[, Mark := as.numeric(substr(Mark, 1, 2))]
  }

  # Exclude zeros and missing marks
  data <- data[!is.na(Mark) & !Mark == 0]

  # Return merged dataset
  data
}
