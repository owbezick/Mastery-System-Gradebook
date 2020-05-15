# MAstery Gradebook application
# Author: Owen Bezick


# Source Libraries
source("libraries.R", local = TRUE)
source("data_intake.R", local = TRUE)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Student View"
  )
  , dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "home", text = "Home", icon = icon("home")) 
      , menuItem(tabName = "viewGrades", text = "View Grades", icon = icon("check-double"))
    )
  )
  , dashboardBody( 
    tabItems(
      tabItem(
        tabName = "home"
        , HTML("<center><h1> Mastery Gradebook Dashboard </h1></center>")
        , column(width = 12
                 , fluidRow(uiOutput("student_profile"), uiOutput("prof_profile")
                 )
        )
        , uiOutput("schedule")
        , uiOutput("authModal")
      )
      , tabItem(
        tabName = "viewGrades"
        , tabBox(width = 12
                 , tabPanel(title = "Exam Grades"
                            , box(width = 12, status = "primary"
                                  , fluidRow(
                                    column(width = 4
                                           , valueBoxOutput("mastery", width = 12)
                                    )
                                    , column(width = 4
                                             , valueBoxOutput("journey", width = 12)
                                    )
                                    , column(width = 4
                                             , valueBoxOutput("apprentice", width = 12)
                                    )
                                  )
                            )
                            , fluidRow(
                              column(width = 12
                                     , box(width = 6, status = "primary"
                                           , DTOutput("exam_grades_dt")
                                     )
                                     , box(width = 6, status = "primary"
                                           , echarts4rOutput("gradeBar")
                                     )
                              )
                            )
                 )
                 , tabPanel(title = "Homework Grades"
                            , fluidRow(
                              column(width = 6
                                     , valueBoxOutput("homework_average", width = 12)
                                     , box(width = 12, status = "primary"
                                           , DTOutput("homework_grades_dt")
                                     )
                              )
                              , column(width = 6
                                       , box(width = 12, status = "primary"
                                             , echarts4rOutput("homeworkScatter")
                                       )
                              )
                            )
                 )
        )
      )
    )
  )
)



# Define server logic 
server <- function(input, output) {
  # Authentication ---- 
  output$authModal <- renderUI({
    showModal(
      modalDialog(title = "Authentication", easyClose = F, footer = actionButton(inputId = "auth_save", label = "Continue")
                  , numericInput(inputId = "student_id"
                                 , label = "Enter your Student ID:"
                                 , value = 0)
      )
    )
  })
  
  # Pulls input information
  is <- reactiveValues(auth = F)
  auth_student_id <- reactive(input$student_id)
  
  # List of valid student idss
  ls_student_id <- student_def %>%
    distinct(student_id) %>% pull()
  
  # Verifies user input
  observeEvent(input$auth_save, {
    if (input$student_id %in% ls_student_id){
      name <- student_def %>%
        filter(student_id == input$student_id) %>% 
        select(first) %>%
        pull()
      showNotification(paste0("Welcome, ",name, "!"), type = "message")
      is$auth <- T
      removeModal()
    } else {
      showNotification(paste(as.character(input$student_id), "not found. Please try again."), type = "error")
    }
  })
  
  # Creates student data frames
  exam_grades <- reactive({
    req(is$auth)
    current_student <- auth_student_id()
    exam_grade <- exam_grade %>%
      filter(student_id == current_student)
  })
  
  homework_grades <- reactive({
    req(is$auth)
    current_student  <- auth_student_id()
    homework_grade <- homework_grade %>%
      filter(student_id == current_student)
  })
  
  # Student Profile ----
  output$student_profile <- renderUI({
    req(is$auth) # requires authentication for viewing
    df <- student_def %>%
      filter(student_id == auth_student_id())
    box(width= 6, title = "Student Information", status = "primary"
        , column(width = 6
                 , fluidRow(
                   img(src= paste0(as.character(df$student_id), ".jpg"))
                 )
        )
        , column(width = 6
                 , fluidRow(
                   HTML(paste0("<b>", paste(df$first, df$last), "</b>"))
                 )
                 , br()
                 , fluidRow(
                   HTML("<b> Student ID: </b>")
                   , df$student_id
                 )
                 , br()
                 , fluidRow(
                   HTML("<b> Email: </b>")
                   , tolower(paste0(substr(df$first, 0, 2), df$last, "@davidson.edu"))
                 )
                 
        )
    )
  })
  
  # Professor Profile ----
  output$prof_profile <- renderUI({
    req(is$auth) # requires authentication for viewing
    df <- student_def %>%
      filter(student_id == auth_student_id())
    
    box(width= 6, title = "Professor Information", status = "primary"
        , column(width = 6
                 , fluidRow(
                   img(src= "mascot.jpg")
                 )
        )
        , column(width = 6
                 , fluidRow(
                   HTML("<b> Dr. Professorson </b>"),
                 )
                 , br()
                 , fluidRow(
                   HTML("<b> Email: </b>"),
                   HTML("drprofessorson@davidson.edu")
                 )
                 , br()
                 , fluidRow(
                   HTML("<b> Office Hours: </b>")
                   , br()
                   , HTML("MWF: 9:30- 11")
                   , br()
                   , HTML("TTh: 1:40-3:00")
                 )
        )
    )
  })
  
  # Schedule ----
  output$gantt <- renderTimevis({
    exams <- exam_def %>%
      mutate(id = paste("Exam", exam_id)) %>%
      select(content = id, start = date)
    homeworks <- homework_def %>%
      mutate(id = paste("Homework", homework_id))%>%
      select(content = id, start = date)
    
    assignments <- rbind(exams,homeworks)
    timevis(assignments)
  })
  
  output$schedule <- renderUI({ 
    req(is$auth)
    fluidRow(column(width = 12
                    , box(width = 12, title = "Assignment Schedule", status = "primary" , timevisOutput("gantt")
                    )
    )
    )
  })
  
  # Exam Grades ----
  output$exam_grades_dt <- renderDT({
    df <- exam_grades() %>%
      select(`Exam ID` = exam_id, `Topic ID` = topic_id, Grade = grade)
    datatable(df, rownames = F)
  })
  output$gradeBar <- renderEcharts4r({
    df <- exam_grades() %>%
      select(grade) %>%
      filter(grade != "NA") %>%
      count(grade)
    
    apprentice <- df %>%
      filter(grade == "A") %>%
      pull()
    journey <-df %>%
      filter(grade == "J") %>%
      pull()
    master <- df %>%
      filter(grade == "M") %>%
      pull()
    
    graph_df <- tibble(A = c(0 + as.numeric(apprentice[1]))
                       , J = c(0 + as.numeric(journey[1]))
                       , M = c(0 + as.numeric(master[1]))
                       , chart = c(""))
    graph_df %>%
      e_chart(chart) %>%
      e_bar("A", name = "Apprentice") %>%
      e_bar("J", name = "Journeyman")  %>%
      e_bar("M", name = "Master") %>%
      e_theme("westeros") %>%
      e_tooltip() %>%
      e_legend(bottom = 0)
  })
  
  output$mastery <- renderValueBox({
    value <- exam_grades() %>%
      filter(grade == "M") %>%
      count() %>%
      pull()
    
    valueBox(value = value, subtitle = "Mastered")
  })
  
  output$journey<- renderValueBox({
    value <- exam_grades() %>%
      filter(grade == "J") %>%
      count() %>%
      pull()
    valueBox(value = value, subtitle = "Journeyman")
  })
  
  output$apprentice <- renderValueBox({
    value <- exam_grades() %>%
      filter(grade == "A") %>%
      count() %>%
      pull()
    valueBox(value = value, subtitle = "Apprentice")
  })
  
  # Homework Grades ----
  output$homework_grades_dt <- renderDT({
    df <- homework_grades()
    df <- df %>%
      select(`Homework ID` = homework_id, Grade= grade)
    datatable(df, rownames = FALSE)
  })
  
  output$homeworkScatter <- renderEcharts4r({
    df <- homework_grades()
    df <- df %>%
      mutate(homework_id = as.character(homework_id))
    df %>%
      e_chart(homework_id) %>%
      e_scatter(grade, symbol_size = 15) %>%
      e_tooltip() %>%
      e_theme('westeros') %>%
      e_legend(show=F)
  })
  
output$homework_average <- renderValueBox({
  value <- homework_grades() %>%
    summarise(avg = mean(grade)) %>% pull()
  valueBox(value, "Homework Average")
})
  
}

# Run the application 
shinyApp(ui = ui, server = server)