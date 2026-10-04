# BDA400 Assignment 6
# Technical Analysis using R - Visualization Phase

# ---------------------------------------------------------
# Step 1: Load Required Packages
# These packages are used to create the Shiny application,
# retrieve stock data, create charts, and calculate indicators.
# ---------------------------------------------------------

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)


# ---------------------------------------------------------
# Step 2: Create the User Interface
# The UI allows the user to select a stock, date range,
# time frame, chart type, and technical indicators.
# ---------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Stock Portfolio Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Input for the stock symbol
      textInput(
        "symbol",
        "Stock Symbol:",
        value = "AAPL"
      ),
      
      # Input for selecting the historical date range
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = "2023-07-01"
      ),
      
      # Input for selecting the data time frame
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      
      # Input for selecting the chart type
      selectInput(
        "chart_type",
        "Select Chart Type:",
        choices = c("Line", "Area")
      ),
      
      # Checkboxes allow indicators to be turned on and off
      checkboxGroupInput(
        "technical_indicators",
        "Technical Indicators:",
        choices = c(
          "Moving Averages",
          "RSI",
          "MACD"
        )
      )
    ),
    
    # Main dashboard output
    mainPanel(
      plotOutput("stock_chart"),
      h4("Trading Signal"),
      verbatimTextOutput("current_signal")
    )
  )
)


# ---------------------------------------------------------
# Step 3: Create the Server
# The server retrieves and processes the selected stock data.
# ---------------------------------------------------------

server <- function(input, output) {
  
  # Retrieve historical stock data from Yahoo Finance
  stock_data <- reactive({
    
    getSymbols(
      input$symbol,
      src = "yahoo",
      from = input$date_range[1],
      to = input$date_range[2],
      auto.assign = FALSE
    )
  })
  
  
  # -------------------------------------------------------
  # Step 4: Prepare Stock Data and Technical Indicators
  # Closing prices are extracted and the SMA, RSI, and MACD
  # indicators are calculated.
  # -------------------------------------------------------
  
  prepared_data <- reactive({
    
    data <- stock_data()
    
    # Convert data when a different time frame is selected
    if (input$time_frame == "Weekly") {
      data <- to.weekly(
        data,
        indexAt = "lastof",
        OHLC = TRUE
      )
    }
    
    if (input$time_frame == "Monthly") {
      data <- to.monthly(
        data,
        indexAt = "lastof",
        OHLC = TRUE
      )
    }
    
    # Extract closing prices
    prices <- as.numeric(Cl(data))
    
    # Create a data frame for plotting
    df <- data.frame(
      Date = as.Date(index(data)),
      Close = prices
    )
    
    # Calculate 20-period and 50-period moving averages
    df$SMA20 <- SMA(prices, n = 20)
    df$SMA50 <- SMA(prices, n = 50)
    
    # Calculate the Relative Strength Index
    df$RSI <- RSI(prices, n = 14)
    
    # Calculate MACD
    macd_values <- MACD(
      prices,
      nFast = 12,
      nSlow = 26,
      nSig = 9
    )
    
    df$MACD <- macd_values[, 1]
    df$MACDSignal <- macd_values[, 2]
    
    
    # -----------------------------------------------------
    # Step 5: Implement Trading Rules
    # A Buy signal occurs when the short moving average
    # crosses above the long moving average.
    # A Sell signal occurs when it crosses below.
    # Otherwise, the signal remains Hold.
    # -----------------------------------------------------
    
    df$Signal <- "Hold"
    
    for (i in 2:nrow(df)) {
      
      if (
        !is.na(df$SMA20[i - 1]) &&
        !is.na(df$SMA50[i - 1]) &&
        !is.na(df$SMA20[i]) &&
        !is.na(df$SMA50[i])
      ) {
        
        # Generate a Buy signal
        if (
          df$SMA20[i - 1] <= df$SMA50[i - 1] &&
          df$SMA20[i] > df$SMA50[i]
        ) {
          df$Signal[i] <- "Buy"
        }
        
        # Generate a Sell signal
        if (
          df$SMA20[i - 1] >= df$SMA50[i - 1] &&
          df$SMA20[i] < df$SMA50[i]
        ) {
          df$Signal[i] <- "Sell"
        }
      }
    }
    
    df
  })
  
  
  # -------------------------------------------------------
  # Step 6: Visualize the Stock Data
  # Create either a line or area chart based on user input.
  # -------------------------------------------------------
  
  output$stock_chart <- renderPlot({
    
    df <- prepared_data()
    
    # Create a line chart
    if (input$chart_type == "Line") {
      
      p <- ggplot(
        df,
        aes(x = Date, y = Close)
      ) +
        geom_line()
      
    } else {
      
      # Create an area chart
      p <- ggplot(
        df,
        aes(x = Date, y = Close)
      ) +
        geom_area(alpha = 0.4)
    }
    
    
    # -----------------------------------------------------
    # Step 7: Overlay Technical Indicators
    # Indicators appear only when selected by the user.
    # -----------------------------------------------------
    
    # Add Moving Averages
    if ("Moving Averages" %in% input$technical_indicators) {
      
      p <- p +
        geom_line(
          aes(y = SMA20),
          linetype = "dashed"
        ) +
        geom_line(
          aes(y = SMA50),
          linetype = "dotted"
        )
    }
    
    
    # Add RSI
    if ("RSI" %in% input$technical_indicators) {
      
      # Scale RSI so it can be displayed on the price chart
      rsi_scaled <- df$RSI / 100 *
        diff(range(df$Close, na.rm = TRUE)) +
        min(df$Close, na.rm = TRUE)
      
      p <- p +
        geom_line(
          aes(y = rsi_scaled),
          linetype = "dotdash"
        )
    }
    
    
    # Add MACD
    if ("MACD" %in% input$technical_indicators) {
      
      # Scale MACD so it can be displayed on the price chart
      macd_range <- range(
        df$MACD,
        na.rm = TRUE
      )
      
      price_range <- range(
        df$Close,
        na.rm = TRUE
      )
      
      macd_scaled <- (
        (df$MACD - macd_range[1]) /
          (macd_range[2] - macd_range[1])
      ) *
        diff(price_range) +
        price_range[1]
      
      p <- p +
        geom_line(
          aes(y = macd_scaled),
          linewidth = 0.8
        )
    }
    
    
    # -----------------------------------------------------
    # Step 8: Add Buy and Sell Annotations
    # Display generated Buy and Sell signals on the chart.
    # -----------------------------------------------------
    
    signal_data <- df[
      df$Signal %in% c("Buy", "Sell"),
    ]
    
    p <- p +
      geom_text(
        data = signal_data,
        aes(
          x = Date,
          y = Close,
          label = Signal
        ),
        vjust = -0.8
      ) +
      labs(
        title = paste(
          input$symbol,
          "Stock Portfolio Dashboard"
        ),
        subtitle = paste(
          "Time Frame:",
          input$time_frame
        ),
        x = "Date",
        y = "Closing Price"
      ) +
      theme_minimal()
    
    print(p)
  })
  
  
  # -------------------------------------------------------
  # Step 9: Display the Current Trading Signal
  # Show the most recent Buy, Sell, or Hold signal.
  # -------------------------------------------------------
  
  output$current_signal <- renderText({
    
    df <- prepared_data()
    
    latest <- tail(df, 1)
    
    paste(
      "Current signal:",
      latest$Signal
    )
  })
}


# ---------------------------------------------------------
# Step 10: Run the Shiny Application
# ---------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
