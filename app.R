library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)

data <- read.csv("data.csv")

data$index <- 1:nrow(data)
data

data$Rn = as.integer(data$Media.Spend)
data

ui <- fluidPage(
    titlePanel("Campaign Search Volumes"),
  
      sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "MediaCampaign",label = "Media Campaign", choices =  c(data$MediaCampaign), selected = "data$MediaCampaign", multiple = F),
        sliderInput(inputId = "RetentionFactor", label = "Retention Factor ", min = 1,max = 10, value= 1,step = 0.1),
            ),
      
      mainPanel(
        plotOutput(outputId = "distPlot"),
        verbatimTextOutput(outputId ="stats"),
      )
    )
  )

server <- function(input, output) {
    
      selectedData <- reactive({
        data %>% filter(data$MediaCampaign==input$MediaCampaign)
      })
      
      output$distPlot <- renderPlot({
        plot(x=selectedData()$index,y=selectedData()$Media.Spend, xlab="week",ylab="Adstock", type="h")
      })
      
      output$stats <- renderPrint({
        summary(selectedData()$Media.Spend)
      })
    }

shinyApp(ui = ui, server = server)