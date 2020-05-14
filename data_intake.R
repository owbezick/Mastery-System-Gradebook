# Data Intake for Mastery Gradebook
# Author: Owen Bezick

student_def <- read_excel("proposed_database.xlsx", sheet = "student_def")
exam_def <- read_excel("proposed_database.xlsx", sheet = "exam_def")
exam_grade <- read_excel("proposed_database.xlsx", sheet = "exam_grade")
homework_def <- read_excel("proposed_database.xlsx", sheet = "homework_def")
homework_grade <- read_excel("proposed_database.xlsx", sheet = "homework_grade")

