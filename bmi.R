library(shiny)
library(data.table)
library(randomForest)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage(
                  "BMI Calculator",
                  tabPanel("Home",
                           sidebarPanel(
                             tags$h3("Inputs"),
                             numericInput("height", label = "Height", value = 175),
                             numericInput("weight", label = "Weight", value = 66),
                             actionButton("submit", "Submit", class = "btn btn-primary")
                           ),
                           mainPanel(
                             tags$label(h3("Status/Output")),
                             verbatimTextOutput("contents"),
                             tableOutput("tabledata")
                           )
                  ),
                  tabPanel("About",
                           titlePanel("About"),
                           div(includeMarkdown("D:/R Projs/about_BMI.md"),
                               align = "justify")
                  )
                )
)

server <- function(input, output, session) {
  datasetInput <- reactive({
    
    bmi <- input$weight / ((input$height/100)^2)
    bmi <- data.frame(bmi)
    names(bmi) <- "BMI"
    print(bmi)
    
  })
  
  output$contents <- renderPrint({
    if (input$submit > 0) {
      isolate("Calculation complete")
    } else {
      return("Server is ready for calculation")
    }
  })
  
  output$tabledata <- renderTable(
    if (input$submit > 0) {
      isolate(datasetInput())
    })
}

shinyApp(ui = ui, server = server)