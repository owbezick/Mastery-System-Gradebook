#
# Author: Owen Bezick
#

# Source Libraries
source("libraries.R", local = TRUE)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Student View"
  )
  , dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "studentProfile", text = "Student Profile", icon = icon("user")) 
      , menuItem(tabName = "viewGrades", text = "View Grades", icon = icon("check-double"))
      , menuItem(tabName = "gradeCalculator", text = "Grade Calculator", icon = icon("calculator")))
  )
  , dashboardBody( 
    tabItems(
      tabItem(
        tabName = "studentProfile"
        )
      , tabItem(
        tabName = "viewGrades"
        , tabBox(width = 12
          , tabPanel(title = "Exam Grades")
          , tabPanel(title = "Homework Grades")
        )
      )
      , tabItem(
        tabName = "gradeCalculator"
      )
    )
  )
)


# Define server logic 
server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)