getMarks <- function(marks, students, modules, simple = F) {

  # Read in marks, plus look-up tables for students and modules
  data <- data.table(read_sheet(marks))

  if(simple == F) {
    students <- data.table(read_sheet(students))
    modules <- data.table(read_sheet(modules))

    # Merge the three tables together
    data <- merge(data, students, by = "Student", all.x = T, all.y = F)
    data <- merge(data, modules, by = c("Code", "Component"))

    # Specify SSP categories
    data[is.na(SSP), SSP := "N"]
    data[, SSP := as.factor(SSP)]

    # Make sure all the variables are the right types
    data[, Shortname := as.factor(Shortname)]
    data[, Module := as.factor(Module)]
    data[, Term := as.factor(Term)]
    data[, Gender := as.factor(Gender)]
    data[, Programme := as.factor(Programme)]
  }
  data[, Code := as.factor(Code)]
  data[, Student := as.factor(Student)]

  # If text has found its way into Mark (most likely as a note after the actual numbers), purge it
  if(!class(data$Mark) == "numeric") {
    data[, Mark := as.numeric(substr(Mark, 1, 2))]
  }

  # Exclude zeros and missing marks
  data <- data[!is.na(Mark) & !Mark == 0]

  # Return merged dataset
  data
}
