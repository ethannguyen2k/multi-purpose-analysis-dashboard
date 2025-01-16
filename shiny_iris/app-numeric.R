library(shiny)
library(data.table)
library(randomForest)

model <- readRDS("D:/R Projs/shiny_iris/model.rds")

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  headerPanel("Iris Predictor"),
    sidebarPanel(
      HTML("<h3>Input Parameters</h3>"),
      numericInput("Sepal.Length", label = "Sepal Length", value = 5.0),
      numericInput("Sepal.Width", label = "Sepal Width", value = 3.6),
      numericInput("Petal.Length", label = "Petal Length", value = 1.4),
      numericInput("Petal.Width", label = "Petal Width", value = 0.2),
      actionButton("submit", "Submit", class = "btn btn-primary")
    ),
    mainPanel(
      tags$label(h3("Status/Output")),
      verbatimTextOutput("contents"),
      tableOutput("tabledata")
    )
)

server <- function(input, output, session) {
  datasetInput <- reactive({
    df <- data.frame(
      Name = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
      Value = as.character(c(input$Sepal.Length, input$Sepal.Width, input$Petal.Length, input$Petal.Width)),
      stringsAsFactors = FALSE
    )
    Species <- 0
    df <- rbind(df, Species)
    input <- transpose(df)
    write.table(input, "input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv("input.csv", header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
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
    }
  )
}

shinyApp(ui = ui, server = server)