  library(shiny)
  library(ggplot2)
  library(plyr)
  library(dplyr)
  library(lubridate)
  library(dplyr)
  
  data <- read.csv("data.csv")
  
  data$index <- 1:nrow(data)
  data
  
  ui <- fluidPage(
    titlePanel("Campaign Search Volumes"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "MediaCampaign",label = "Media Campaign", choices =  c(data$MediaCampaign), selected = "data$MediaCampaign", multiple = F),
        sliderInput(inputId = "RetentionFactor", label = "Retention Factor ", min = 0,max = 1, value= 0.1,step = 0.1),),
      
      mainPanel(
        plotOutput(outputId = "Plot"),
        tableOutput("TbEfficiencies"),
      )
    )
  )
  
  server <- function(input, output) {
    
    GetRFInput <- function(){
      return(IPRF <- input$RetentionFactor)
    }
    
      selectedData <- reactive({
      data %>% filter(data$MediaCampaign==input$MediaCampaign)
    })
    
    myAdstock <- 0.0;
    AdstockIterative <- function(MSpend,RF,Week)
    {
      myAdstock <- MSpend + RF * myAdstock
      return(myAdstock)
    }
    
    AdstockRecursive  <- function(MSpend,RF,week)
    {
      #return(MSpend + (RF * AdstockIterative(MSpend,RF, week-1)))
      return(MSpend + (RF * lag(AdstockIterative(MSpend,RF, week))))
    }  
    
    output$Plot <- renderPlot({
      Adstock = AdstockRecursive(selectedData()$Media.Spend,input$RetentionFactor,selectedData()$index)
      plot(x=selectedData()$weekID,y=Adstock, xlab="Week Number for Campaign",ylab="Adstock", type="h")
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