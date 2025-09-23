
install.packages(c("shiny" , "shinythemes" ,"quantmod" ,"ggplot2" ,"plotly"))
# --------------------
# Load Required Packages
# --------------------
# This section loads our "toolkits" into our R session so we can use them.
library(shiny)
library(shinythemes)
library(quantmod)
library(ggplot2)
library(plotly)

# This is your NEW, designed "Menu"
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("Stock Market Analysis Dashboard"),
  
  sidebarLayout(
    
    # sidebarPanel contains all our input controls
    sidebarPanel(
      helpText("Enter a stock ticker from Yahoo Finance and select a date range."),
      
      # Item 1: Text input. Notice the comma AFTER its closing parenthesis.
      textInput(inputId = "ticker",
                label = "Ticker Symbol",
                value = "NVDA"), # Default to NVIDIA
      
      # Item 2: Date range input.
      dateRangeInput(inputId = "dates",
                     label = "Select Date Range",
                     start = "2024-01-01",
                     end = Sys.Date()), # Default to this year
      
      # Item 3: Action button. Label is now INSIDE the parentheses.
      actionButton(inputId = "get_data",
                   label = "Analyze")
    ),
    
    # Main panel for displaying the chart
    mainPanel(
      h3(textOutput("chart_title")),
      
      # Using plotlyOutput to match the interactive chart we'll make in the server
      plotlyOutput(outputId = "price_chart") 
    )
  )
)
 
# Part 2: The Server - The "Kitchen"
# ----------------------------------------------------
server <- function(input, output) {
  
  # This is a "reactive" expression. It's a recipe that only runs
  # when the "Analyze" button is clicked.
  stock_data <- eventReactive(input$get_data, {
    
    # Show a notification that we are getting data
    showNotification("Fetching data from Yahoo Finance...", type = "message")
    
    # Get the stock data using the user's chosen ticker and dates
    getSymbols(input$ticker, 
               src = "yahoo", 
               from = input$dates[1], 
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  # Create a dynamic title for the chart
  output$chart_title <- renderText({
    # This title will only appear after the button is clicked
    req(stock_data())
    paste("Displaying data for", input$ticker)
  })
  
  # Create the interactive plot
  output$price_chart <- renderPlotly({
    # We need the data first, so we call stock_data()
    the_data <- stock_data()
    
    # Convert the data into a format ggplot2 understands (a data frame)
    # This is the NEW, corrected line
    data_df <- data.frame(Date = index(the_data), Adjusted = as.numeric(Ad(the_data)))
    
    # The ggplot recipe for our chart
    p <- ggplot(data_df, aes(x = Date, y = Adjusted)) +
      geom_line(color = "#00BFFF") + # Draw a nice blue line
      labs(title = paste(input$ticker, "Adjusted Closing Price"),
           x = "Date",
           y = "Price (USD)") +
      theme_minimal()
    
    # Convert the ggplot object to an interactive plotly object
    ggplotly(p)
  })
  
}

# ----------------------------------------------------
# Part 3: Run the Application (The "Manager")
# ----------------------------------------------------
shinyApp(ui = ui, server = server)

