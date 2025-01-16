library(shiny)
library(shinythemes)
library(data.table)
library(randomForest)
library(RCurl)

weather <- read.csv("https://raw.githubusercontent.com/dataprofessor/data/refs/heads/master/weather-weka.csv", stringsAsFactors = T)

model <- randomForest(
  play ~ .,
  data = weather,
  ntree = 500,
  mtry = 4,
  importance = TRUE
)

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  headerPanel("Play Golf or not"),
  sidebarPanel(
    h3("Input Parameter"),
    selectInput("outlook", label = "Outlook",
                choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"),
                selected = "Sunny"),
    sliderInput("temperature", label = "Temprature",
                min = 64, max = 86, value = 70),
    sliderInput("humidity", label = "Humidity",
                min = 65, max = 96, value = 90),
    selectInput("windy", label = "Windy",
                choices = list("Yes" = "TRUE", "No" = "FALSE"),
                selected = "Yes"),
    actionButton("submit", label = "Submit", class = "btn btn-porimary")
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
      Name = c("outlook", "temperature", "humidity", "windy"),
      Value = as.character(c(input$outlook, input$temperature, input$humidity, input$windy)),
      stringsAsFactors = FALSE
    )
    play <- "play"
    df <- rbind(df, play)
    input <- transpose(df)
    write.table(input, "input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv("input.csv", header = TRUE)
    test$outlook <- factor(test$outlook, levels = c("overcast", "rainy", "sunny"))
    
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
    })
}

shinyApp(ui = ui, server = server)