library(tidyverse)
library(jsonlite)
library(lubridate)
library(httr)
library(hrbrthemes)
library(styler)
library(shiny)

#usethis::edit_r_environ("project")
readRenviron(".Renviron")


ui <- fluidPage(title = "Stock Info",
  tabsetPanel(
    tabPanel(title = "Stock",  #First page, it will be esstinally the home page
    titlePanel("Stock Inforamtion"),
    sidebarLayout(
      sidebarPanel(
        textInput("txt", label = h5("Please enter a ticker symbol below:")), #USer inputs ticker
        verbatimTextOutput("ticker"),
        selectInput("interval", "Select Per Week or Month", choices = c("---", "WEEKLY","MONTHLY")), #View data on a weekly or monthly basis
        selectInput("info", label = "Choose one option below",
                     choices = c("---","open",
                                 "high",
                                 "low",
                                 "close")), #First otion to choose from
        selectInput("info2", label = "Choose another option below",
                    choices = c("---","open",
                                "high",
                                "low",
                                "close")), #Second option to choose from
        sliderInput("num", label = "Select a range to focus on: (1 = current to n = oldest)",
                    value = c(1,500), min = 1, max = 500), #Slider input so User can select a range they wish to view
        checkboxInput("line", label = "Regression Line"), #Add regression line option
        checkboxInput("CI", label = "Confidence Interval") #Add CI to regression line option
      ),
      mainPanel(
        plotOutput("TS") #Display the time series chart
        
      )
    )
    ),
    tabPanel(title = "About", #the about page which describes the app and its functionality
      p("Hello! Welcome to the InfoStock App. The purpose of this app is to give 
        a bigneer investor the oppurtunity to look up information for a particular stock. 
        This app will give you the oppurtunity to look at the open, close, low, and high prices
        of a stock in either a weekly or monthly basis. This app uses an API from
        Alpha Vintage that provides us the stock information for any ticker for the last
        20 years. 
        "),p("The App is super user frindly and dose not require any particulr knowledge 
        about stocks. The only thing the user needs to provide is a valid ticker label (stock
        name). Once the users has picked a ticker, they can choose to see the stock information
        on a weekly or monthly basis. The user will then have the ability to choose between any
        twp combination of either open, close, low, and high prices of a stock. A time series
        graph will then appear with the two combinations the user has choosen. The user will also
        have the ability to zoom into a particluar time interval. The last
        functionality this app will have is the feature to add a regression (with or without
        a confidence interval)
        line onto the chart. The reason we added a regression line functionality, is to allow 
        user to see the general trend of a stock and to better detect and
        visualize patterns. The sliding feature will 
        allow the user to choose the range they are intrested in (1 is the most current and n is the lastest year).
        We hope that this app will be useful to new stock investors as it is user friendly 
        and provides on demmand data.")   
             )
  )
)

server <- function(input, output, session) {
  readRenviron(".Renviron") #to read api token 
  
  opt <- c("open",
           "high",
           "low",
           "close") # Options to display info
  
  observeEvent(input$info,{updateSelectInput(session, "info2",label = "Select one option below",
                    choices = setdiff(opt,input$info)) #Update the second select option depending on what the first choice was
  })
  observe({
    freezeReactiveValue(input,"num")
    freezeReactiveValue(input,"info")
    observeEvent(input$info,{updateSliderInput(session, "num", label = "Select a range to focus on: (1 = current to n = oldest)",
                                              min = 1, max = length(data2()[,1]),
                                              value = c(1,length(data2()[,1])))}) 
    #Update the slider depending on the availble weeks/moths of a ticker available
  })
  
  data2 <- reactive({ #Purpose: to update data as values are insrted by user
    GET(
      "https://www.alphavantage.co/query",
      query = list(
        `function` = str_glue("TIME_SERIES_{type}", type = input$interval),
        symbol = input$txt,
        apikey = Sys.getenv("STOCK_TOKEN"),
        datatype = "csv"
      ) #Information to connect to API
    )%>% 
      stop_for_status() %>% 
      content(as = "text") %>% 
      str_remove_all("\\n") %>% #the document is in text form
      str_split("\\r",simplify = TRUE) %>% 
      str_split(",", simplify = TRUE) %>% 
      as.data.frame() %>% #convert doc to dataframe
      rename(time = V1, open = V2, high = V3, low = V4, close = V5, volume = V6)
  }) #build a dataframe for easier manipulation later on
  
  data <- eventReactive(c(input$info,input$interval), { #Update data as the first choice and interval options are inserted by the user
    GET(
      "https://www.alphavantage.co/query",
      query = list(
        `function` = str_glue("TIME_SERIES_{type}", type = input$interval),
        symbol = input$txt,
        apikey = Sys.getenv("STOCK_TOKEN"),
        datatype = "csv"
      ) #connect to the API
    )%>% 
      stop_for_status() %>% 
      content(as = "text") %>% 
      str_remove_all("\\n") %>% 
      str_split("\\r",simplify = TRUE) %>% 
      str_split(",", simplify = TRUE) %>% 
      as.data.frame() %>% #convert the text doc to a datafrem
      rename(time = V1, open = V2, high = V3, low = V4, close = V5, volume = V6)
  })
  
  output$TS <- renderPlot({
    data <- data()[-c(1, length(time)-1),] #delete the first and last rows, they are irrlevant and dont contain info
    data <- data[input$num[1]:input$num[2],] #filter rows depending on how the user wants to view the data
    
    graph <- ggplot(data, 
           aes(x = as.Date(time))) +
      geom_line(aes(y = as.numeric(data[,input$info]), color = input$info)) + #add line for first and 2nd option
      geom_line(aes(y = as.numeric(data[,input$info2]), color = input$info2)) +
      xlab("")+
      labs(color = "Stock Info",
           x = input$interval,
           y = "price($)", #Add chart labels
           title = paste(input$interval, "stock information for", input$txt))+
      theme_ipsum() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    if (input$line == TRUE){ #check if user has selected Reg Line option
      graph +
        geom_smooth(se = input$CI, aes(y = as.numeric(data[,input$info]))) #add regresion line depnding if the user wants one or not
    } else{
      graph
    }
    
  })
}

shinyApp(ui, server)
