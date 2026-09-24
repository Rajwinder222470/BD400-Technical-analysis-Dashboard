# BDA400 - Assignment 6
# Technical Analysis using R - Visualization Phase
# Student: Rajwinder Kaur

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

ui <- fluidPage(
  titlePanel("Technical Analysis Portfolio Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "stock", "Select Stock:",
        choices = c(
          "Apple (AAPL)" = "AAPL",
          "Microsoft (MSFT)" = "MSFT",
          "Alphabet (GOOGL)" = "GOOGL",
          "Amazon (AMZN)" = "AMZN",
          "NVIDIA (NVDA)" = "NVDA"
        ),
        selected = "AAPL"
      ),
      dateRangeInput(
        "dates", "Select Date Range:",
        start = Sys.Date() - 365,
        end = Sys.Date()
      ),
      selectInput(
        "timeframe", "Time Frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      selectInput(
        "chart_type", "Chart Type:",
        choices = c("Line", "Area", "Candlestick"),
        selected = "Line"
      ),
      checkboxGroupInput(
        "indicators", "Technical Indicators:",
        choices = c("SMA" = "SMA", "EMA" = "EMA",
                    "RSI" = "RSI", "MACD" = "MACD"),
        selected = "SMA"
      ),
      numericInput(
        "short_ma", "Short Moving Average:",
        value = 20, min = 2, max = 100
      ),
      numericInput(
        "long_ma", "Long Moving Average:",
        value = 50, min = 3, max = 200
      ),
      checkboxInput(
        "show_signals", "Show Buy/Sell Signals",
        value = TRUE
      )
    ),
    mainPanel(
      h3(textOutput("stock_title")),
      textOutput("status"),
      plotOutput("stock_plot", height = "500px"),
      conditionalPanel(
        condition = "input.indicators.indexOf('RSI') >= 0",
        h4("Relative Strength Index (RSI)"),
        plotOutput("rsi_plot", height = "220px")
      ),
      conditionalPanel(
        condition = "input.indicators.indexOf('MACD') >= 0",
        h4("MACD"),
        plotOutput("macd_plot", height = "240px")
      ),
      h4("Latest Market Information"),
      tableOutput("summary_table")
    )
  )
)

server <- function(input, output, session) {

  stock_data <- reactive({
    validate(
      need(input$dates[1] < input$dates[2],
           "Please select a valid date range.")
    )

    tryCatch({
      data <- getSymbols(
        input$stock,
        src = "yahoo",
        from = input$dates[1],
        to = input$dates[2],
        auto.assign = FALSE
      )

      validate(
        need(NROW(data) > 0, "No stock data were returned.")
      )

      data
    }, error = function(e) {
      validate(
        need(FALSE,
             paste(
               "Stock data could not be downloaded.",
               "Please check the symbol, date range, or internet connection."
             ))
      )
    })
  })

  timeframe_data <- reactive({
    x <- stock_data()

    if (input$timeframe == "Weekly") {
      x <- to.weekly(x, indexAt = "lastof", drop.time = TRUE)
    }

    if (input$timeframe == "Monthly") {
      x <- to.monthly(x, indexAt = "lastof", drop.time = TRUE)
    }

    x
  })

  plot_data <- reactive({
    x <- timeframe_data()

    validate(
      need(
        NROW(x) > input$long_ma,
        paste(
          "Not enough observations.",
          "Choose a longer date range or a smaller long moving average."
        )
      ),
      need(
        input$short_ma < input$long_ma,
        "Short moving average must be smaller than long moving average."
      )
    )

    df <- data.frame(
      Date = as.Date(index(x)),
      Open = as.numeric(Op(x)),
      High = as.numeric(Hi(x)),
      Low = as.numeric(Lo(x)),
      Close = as.numeric(Cl(x))
    )

    df$SMA <- SMA(df$Close, n = input$short_ma)
    df$LongSMA <- SMA(df$Close, n = input$long_ma)
    df$EMA <- EMA(df$Close, n = input$short_ma)
    df$RSI <- RSI(df$Close, n = 14)

    macd_values <- MACD(
      df$Close,
      nFast = 12,
      nSlow = 26,
      nSig = 9,
      maType = "EMA",
      percent = FALSE
    )

    df$MACD <- macd_values[, 1]
    df$MACDSignal <- macd_values[, 2]

    difference <- df$SMA - df$LongSMA
    previous_difference <- c(NA, head(difference, -1))

    df$Signal <- "Hold"

    df$Signal[
      difference > 0 &
        previous_difference <= 0 &
        !is.na(previous_difference)
    ] <- "Buy"

    df$Signal[
      difference < 0 &
        previous_difference >= 0 &
        !is.na(previous_difference)
    ] <- "Sell"

    df
  })

  output$stock_title <- renderText({
    paste(input$stock, "-", input$timeframe, "Technical Analysis")
  })

  output$status <- renderText({
    df <- plot_data()
    latest <- tail(df, 1)

    paste(
      "Latest close:",
      round(latest$Close, 2),
      "| Current MA position:",
      ifelse(
        latest$SMA > latest$LongSMA,
        "Short MA above Long MA",
        "Short MA below Long MA"
      )
    )
  })

  output$stock_plot <- renderPlot({
    df <- plot_data()
    p <- ggplot(df, aes(x = Date))

    if (input$chart_type == "Line") {
      p <- p +
        geom_line(aes(y = Close), linewidth = 0.8)
    }

    if (input$chart_type == "Area") {
      p <- p +
        geom_area(aes(y = Close), alpha = 0.30) +
        geom_line(aes(y = Close), linewidth = 0.7)
    }

    if (input$chart_type == "Candlestick") {
      p <- p +
        geom_segment(
          aes(x = Date, xend = Date, y = Low, yend = High)
        ) +
        geom_segment(
          aes(x = Date, xend = Date, y = Open, yend = Close),
          linewidth = 3
        )
    }

    if ("SMA" %in% input$indicators) {
      p <- p +
        geom_line(
          aes(y = SMA, linetype = "Short SMA"),
          linewidth = 0.8, na.rm = TRUE
        ) +
        geom_line(
          aes(y = LongSMA, linetype = "Long SMA"),
          linewidth = 0.8, na.rm = TRUE
        )
    }

    if ("EMA" %in% input$indicators) {
      p <- p +
        geom_line(
          aes(y = EMA, linetype = "EMA"),
          linewidth = 0.8, na.rm = TRUE
        )
    }

    if (input$show_signals) {
      buy_points <- df[df$Signal == "Buy", ]
      sell_points <- df[df$Signal == "Sell", ]

      if (nrow(buy_points) > 0) {
        p <- p +
          geom_point(
            data = buy_points,
            aes(y = Close, shape = "Buy"),
            size = 3
          ) +
          geom_text(
            data = buy_points,
            aes(y = Close, label = "BUY"),
            vjust = -1, size = 3
          )
      }

      if (nrow(sell_points) > 0) {
        p <- p +
          geom_point(
            data = sell_points,
            aes(y = Close, shape = "Sell"),
            size = 3
          ) +
          geom_text(
            data = sell_points,
            aes(y = Close, label = "SELL"),
            vjust = 1.8, size = 3
          )
      }
    }

    p +
      labs(
        title = paste(input$stock, input$timeframe, "Price Dashboard"),
        subtitle = paste(
          "MA crossover:",
          input$short_ma, "period vs.",
          input$long_ma, "period"
        ),
        x = "Date",
        y = "Stock Price",
        linetype = "Indicator",
        shape = "Trading Signal"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })

  output$rsi_plot <- renderPlot({
    df <- plot_data()

    ggplot(df, aes(x = Date, y = RSI)) +
      geom_line(linewidth = 0.8, na.rm = TRUE) +
      geom_hline(yintercept = 70, linetype = "dashed") +
      geom_hline(yintercept = 30, linetype = "dashed") +
      labs(
        x = "Date", y = "RSI",
        title = "14-Period RSI"
      ) +
      theme_minimal()
  })

  output$macd_plot <- renderPlot({
    df <- plot_data()

    ggplot(df, aes(x = Date)) +
      geom_line(
        aes(y = MACD, linetype = "MACD"),
        linewidth = 0.8, na.rm = TRUE
      ) +
      geom_line(
        aes(y = MACDSignal, linetype = "Signal"),
        linewidth = 0.8, na.rm = TRUE
      ) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      labs(
        x = "Date",
        y = "MACD",
        title = "Moving Average Convergence Divergence",
        linetype = "Series"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })

  output$summary_table <- renderTable({
    df <- plot_data()
    latest <- tail(df, 1)

    data.frame(
      Measure = c(
        "Stock",
        "Latest Date",
        "Closing Price",
        "Short SMA",
        "Long SMA",
        "EMA",
        "RSI",
        "Latest Trading Signal"
      ),
      Value = c(
        input$stock,
        as.character(latest$Date),
        round(latest$Close, 2),
        round(latest$SMA, 2),
        round(latest$LongSMA, 2),
        round(latest$EMA, 2),
        round(latest$RSI, 2),
        latest$Signal
      )
    )
  }, striped = TRUE)
}

shinyApp(ui = ui, server = server)
