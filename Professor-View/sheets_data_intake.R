library(googlesheets4)

gs_auth(new_user = TRUE)

gradebook <- gs4_create("Mastery Grading System Professor View")

# Init Data read in ----
student_def <- read_excel("proposed_database.xlsx", sheet = "student_def")
exam_def <- read_excel("proposed_database.xlsx", sheet = "exam_def")
exam_grade <- read_excel("proposed_database.xlsx", sheet = "exam_grade")
homework_def <- read_excel("proposed_database.xlsx", sheet = "homework_def")
homework_grade <- read_excel("proposed_database.xlsx", sheet = "homework_grade")

sheet_write(student_def, gradebook, "Students")
sheet_write(exam_def, gradebook, "Exams")
sheet_write(exam_grade, gradebook, "Exam Grades")
sheet_write(homework_def, gradebook, "Homeworks")
sheet_write(homework_grade, gradebook, "Homework Grades")

student_def <- read_sheet(gradebook, "Students")
exam_def<- read_sheet(gradebook, "Exams")
exam_grade<- read_sheet(gradebook, "Exam Grades")
homework_def<- read_sheet(gradebook, "Homeworks")
homework_grade<- read_sheet(gradebook, "Homework Grades")

# Reactive Values
reactive <- reactiveValues(exam_def = exam_def, exam_grade = exam_grade,homework_def = homework_def, homework_grade = homework_grade)

exam_grades <- reactive({
  merge(reactive$exam_def, reactive$exam_grade) %>% merge(student_def) %>%
    mutate(firstLast = paste(first, last)) %>%
    select(firstLast, exam_id, topic_id, grade)
})

homework_grades <- reactive({
  merge(reactive$homework_def, reactive$homework_grade) %>% merge(student_def) %>%
    mutate(firstLast = paste(first, last)) %>%
    select(firstLast, last, homework_id, grade)
})