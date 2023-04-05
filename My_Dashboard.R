#### My Dashboard ####

library(shiny)
library(shinydashboard)
install.packages("flexdashboard")
library(flexdashboard)
install.packages("here")
install.packages("thematic")
install.packages("tm")

from <- c("hola", "chau","que")
message <- c("dos", "tres", "uno")

messageData <- data.frame(from,message)

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Sebs",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Sales Dept",
                                 message = "Sales are steady this month."),
                               
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = "11:45"),
                               
                               messageItem(
                                 from = "Support",
                                 message = "The new server is ready.",
                                 icon = icon("fire"),
                                 time = "2022-07-05")
                               ),
                  #dropdownMenuOutput("messageMenu")
                  dropdownMenu(type = "notifications", badgeStatus = "danger",
                               notificationItem(
                                 text = "5 new users today",
                                 icon("users"),
                                 status = "danger"
                               ),
                               notificationItem(
                                 text = "12 items delivered",
                                 icon("truck"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "Server load at 86%",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning")
                               ),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Documentation"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Project X"
                               ),
                               taskItem(value = 75, color = "yellow",
                                        "Server deployment"
                               ),
                               taskItem(value = 80, color = "red",
                                        "Overall project")
                  )
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dna")),
      menuItem("Widgets", tabName = "widgets", icon = icon("seedling"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  ),
  
  # Second tab content
  tabItem(tabName = "widgets",
          h2("Widgets tab content")
      )
    )
  ))

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  #output$messageMenu <- renderMenu({
    #msgs <- apply(messageData, 1, function(row) {
     # messageItem(from = row[["from"]], message = row[["message"]])
   # })
    #})
}

shinyApp(ui, server)























