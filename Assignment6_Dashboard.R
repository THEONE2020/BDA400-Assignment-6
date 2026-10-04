# BDA400 Assignment 6
# Technical Analysis using R - Visualization Phase

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

ui <- fluidPage(
  
  titlePanel("Stock Portfolio Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput(
        "symbol",
        "Stock Symbol:",
        value = "AAPL"
      ),
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = "2023-07-01"
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      
      selectInput(
        "chart_type",
        "Select Chart Type:",
        choices = c("Line", "Area")
      ),
      
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
    
    mainPanel(
      plotOutput("stock_chart"),
      h4("Trading Signal"),
      verbatimTextOutput("current_signal")
    )
  )
)


server <- function(input, output) {
  
  stock_data <- reactive({
    
    getSymbols(
      input$symbol,
      src = "yahoo",
      from = input$date_range[1],
      to = input$date_range[2],
      auto.assign = FALSE
    )
  })
  
  
  prepared_data <- reactive({
    
    data <- stock_data()
    
    if (input$time_frame == "Weekly") {
      data <- to.weekly(data, indexAt = "lastof", OHLC = TRUE)
    }
    
    if (input$time_frame == "Monthly") {
      data <- to.monthly(data, indexAt = "lastof", OHLC = TRUE)
    }
    
    prices <- as.numeric(Cl(data))
    
    df <- data.frame(
      Date = as.Date(index(data)),
      Close = prices
    )
    
    df$SMA20 <- SMA(prices, n = 20)
    df$SMA50 <- SMA(prices, n = 50)
    df$RSI <- RSI(prices, n = 14)
    
    macd_values <- MACD(
      prices,
      nFast = 12,
      nSlow = 26,
      nSig = 9
    )
    
    df$MACD <- macd_values[, 1]
    df$MACDSignal <- macd_values[, 2]
    
    df$Signal <- "Hold"
    
    for (i in 2:nrow(df)) {
      
      if (
        !is.na(df$SMA20[i - 1]) &&
        !is.na(df$SMA50[i - 1]) &&
        !is.na(df$SMA20[i]) &&
        !is.na(df$SMA50[i])
      ) {
        
        if (
          df$SMA20[i - 1] <= df$SMA50[i - 1] &&
          df$SMA20[i] > df$SMA50[i]
        ) {
          df$Signal[i] <- "Buy"
        }
        
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
  
  
  output$stock_chart <- renderPlot({
    
    df <- prepared_data()
    
    if (input$chart_type == "Line") {
      
      p <- ggplot(
        df,
        aes(x = Date, y = Close)
      ) +
        geom_line()
      
    } else {
      
      p <- ggplot(
        df,
        aes(x = Date, y = Close)
      ) +
        geom_area(alpha = 0.4)
    }
    
    
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
    
    
    if ("RSI" %in% input$technical_indicators) {
      
      rsi_scaled <- df$RSI / 100 *
        diff(range(df$Close, na.rm = TRUE)) +
        min(df$Close, na.rm = TRUE)
      
      p <- p +
        geom_line(
          aes(y = rsi_scaled),
          linetype = "dotdash"
        )
    }
    
    
    if ("MACD" %in% input$technical_indicators) {
      
      macd_range <- range(df$MACD, na.rm = TRUE)
      price_range <- range(df$Close, na.rm = TRUE)
      
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
  
  
  output$current_signal <- renderText({
    
    df <- prepared_data()
    
    latest <- tail(df, 1)
    
    paste(
      "Current signal:",
      latest$Signal
    )
  })
}


shinyApp(ui = ui, server = server)