library(shiny)

data("airquality")

ui <- fluidPage(
  titlePanel("Ozone level"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "bins",
                  label = "Number of bins",
                  min = 1, max = 50, value = 30, step = 1)
    ),
    mainPanel(
      plotOutput(outputId = "distplot")
    )
  )
)

server <- function(input, output) {
  
  output$distplot <- renderPlot({
    x <- airquality$Ozone
    x <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#dddddd", border = "black",
         xlab = "Ozone level",
         main = "Histogram of Ozone level")
  })

}

shinyApp(ui = ui, server = server)