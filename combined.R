library(shiny)
library(data.table)
library(randomForest)
library(shinythemes)
library(RCurl)
library(dplyr)

data("airquality")
weather <- read.csv("https://raw.githubusercontent.com/dataprofessor/data/refs/heads/master/weather-weka.csv",
                    stringsAsFactors = T)

weathermodel <- randomForest(
  play ~ .,
  data = weather,
  ntree = 500,
  mtry = 4,
  importance = TRUE
)

ui <- navbarPage(
  theme = shinytheme("cyborg"),
  title = "Multi-Purpose Analytics Dashboard",
  
  tabPanel(
    "Ozone Analysis",
      sidebarLayout(
        sidebarPanel(
          sliderInput(inputId = "ozone_bins",
                      label = "Number of bins",
                      min = 1, max = 50, value = 30, step = 1)
        ),
        mainPanel(
          plotOutput(outputId = "ozone_plot")
        )
      )
  ),
  
  tabPanel(
    "Golf Weather Predictor",
      sidebarPanel(
        h3("Weather Parameter"),
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
        actionButton("weather_submit", label = "Submit", class = "btn btn-porimary")
      ),
      mainPanel(
        tags$label(h3("Prediction Results")),
        verbatimTextOutput("weather_status"),
        tableOutput("weather_prediction")
      )
  ),
  
  tabPanel(
    "BMI Calculator",
      sidebarLayout(
        sidebarPanel(
          h3("Body Measurements"),
          numericInput("height", label = "Height", value = 175),
          numericInput("weight", label = "Weight", value = 66),
          actionButton("bmi_submit", "Submit", class = "btn btn-primary")
        ),
        mainPanel(
          h3("BMI Results"),
          verbatimTextOutput("bmi_status"),
          tableOutput("bmi_result")
        )
      )
  ),
  
  tabPanel(
    "Name Generator",
    sidebarLayout(
      sidebarPanel(
        h3("Name Inputs"),
        textInput("firstname", "Given Name",""),
        textInput("lastname", "SurName","")
      ),
      mainPanel(
        h3("Full Name"),
        verbatimTextOutput("fullname")
      )
    )
  )
  
)

server <- function(input, output, session) {
  
  output$ozone_plot <- renderPlot({
    x <- airquality$Ozone
    x <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$ozone_bins + 1)
    
    hist(x, breaks = bins, col = "#dddddd", border = "black",
         xlab = "Ozone level",
         main = "Histogram of Ozone level")
  })
  
  weather_prediction <- reactive({
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
    
    Output <- data.frame(
      Prediction=predict(weathermodel,test), 
      round(predict(weathermodel,test,type="prob"), 3)
      )
    return(Output)
  })
  
  output$weather_status <- renderPrint({
    if (input$weather_submit > 0) {
      "Calculation complete"
    } else {
      "Ready for calculation"
    }
  })
  
  output$weather_prediction <- renderTable(
    if (input$weather_submit > 0) {
      isolate(weather_prediction())
    })
  
  bmi_calculation <- reactive({
    
    bmi <- input$weight / ((input$height/100)^2)
    data.frame(
      BMI = round(bmi, 2),
      Category = case_when(
        bmi < 18.5 ~ "Underweight",
        bmi < 25 ~ "Normal weight",
        bmi < 30 ~ "Overweight",
        TRUE ~ "Obese"
      )
    )
  })
  
  output$bmi_status <- renderPrint({
    if (input$bmi_submit > 0) {
      "Calculation complete"
    } else {
      "Ready for calculation"
    }
  })
  
  output$bmi_result <- renderTable(
    if (input$bmi_submit > 0) {
      isolate(bmi_calculation())
    })
  
  output$fullname <- renderText({
    paste(input$firstname, input$lastname)
  })
  
}

shinyApp(ui = ui, server = server)