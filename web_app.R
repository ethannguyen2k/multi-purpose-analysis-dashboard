library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage(
                  "Shiny App",
                  tabPanel("Name",
                           sidebarPanel(
                             tags$h3("Inputs"),
                             textInput("txt1", "Given Name",""),
                             textInput("txt2", "SurName","")
                           ),
                           mainPanel(
                             h1("The Name"),
                             h3("Output"),
                             verbatimTextOutput("txtout")
                           )
                           ),
                  tabPanel("Navbar2", "blank"),
                  tabPanel("Navbar3", "blank")
                )
                )

# Define server function
server <- function(input, output) {
  output$txtout <- renderText({
    paste(input$txt1, input$txt2, sep=" ")
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)