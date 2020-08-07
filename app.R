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
      sliderInput(inputId = "RetentionFactor", label = "Retention Factor ", min = 1,max = 10, value= 1,step = 0.1),),
    
    mainPanel(
      plotOutput(outputId = "distPlot"),
      tableOutput("TbEfficiencies"),
    )
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    data %>% filter(data$MediaCampaign==input$MediaCampaign)
  })
  
  
  output$distPlot <- renderPlot({
    for (i in 1:nrow(data))
    {
      j <- lag(i, n = 1L, default = NA, order_by = NULL)
      PrvWeekMediaSpend <- data[j,2]
    }
    
    ###Adstock (in week n) = Media Spend (in week n) + [ RF x Adstock (in week n-1) ]
    #RF <- input$RetentionFactor * data()$PrvAd
    #cbind(data, RF)
    #Adstock <- data()$Media.Spend + data()$RF
    
    RF = selectedData()$Media.Spend + (input$RetentionFactor * selectedData()$Media.Spend)
    plot(x=selectedData()$index,y=RF, xlab="Week",ylab="Adstock", type="h")
  })
  
  Values <- reactive({
    
    data.frame(
      Name = c("Highest Search Volume",
               "Lowest Search Volume",
               "Average Search Volume"),
      Value = as.character(c(paste(max(selectedData()$Media.Spend)),
                             paste(min(selectedData()$Media.Spend)),
                             paste(mean(selectedData()$Media.Spend))
      )),
      
      stringsAsFactors = FALSE)
    
  })
  
  output$TbEfficiencies <- renderTable({
    Values()
  })    
  
}


shinyApp(ui = ui, server = server)