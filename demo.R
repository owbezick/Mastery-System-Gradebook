install.packages("googlesheets4")

library(googlesheets4)

gs_auth(new_user = TRUE)

gradebook <- gs4_create("Mastery Grading System Professor View")

students <- data.frame(
  student_id = c(1:3),
  student_first_name = c("Michael","Dwight","Jim"),
  student_last_name = c("Scott", "Shcrute", "Halpert")
)

exams <- data.frame(
  exam_id = 1,
  first_topic = 1,
  last_topic = 3,
  due_date = as.Date("2019-10-10")
)

exam_grades <- data.frame(
  exam_id = 1,
  student_id = 1,
  topic_id = 1,
  grade = "M"
)

homeworks <- data.frame(
  hw_id = 1,
  description = "Homework 1",
  due_data = as.Date("2019-10-11")
)

homework_grades <- data.frame(
  student_id = 1,
  hw_id = 1,
  grade = 95
)

sheet_write(students, gradebook, "Students")
sheet_write(exams, gradebook, "Exams")
sheet_write(exam_grades, gradebook, "Exam Grades")
sheet_write(homeworks, gradebook, "Homeworks")
sheet_write(homework_grades, gradebook, "Homework Grades")



