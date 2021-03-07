library(shiny)
library(quantmod)
library(dplyr)
library(forecast)
library(tseries)
library(astsa)
library(dygraphs)
library(TTR)
library(DT)

shinyServer(function(input, output, session) {
    SYMs <- TTR::stockSymbols(exchange = c('NASDAQ'))
    nasdaq.syms <- dplyr::select(SYMs, Name, NASDAQ.Symbol)
    
    updateSelectizeInput(
        session,
        'nasdaq.symbols',
        choices = nasdaq.syms$NASDAQ.Symbol,
        server = TRUE,
        selected = c('FB','AAPL','AMZN','NFLX','GOOG')
    )
    
    output$validation <- renderText({
        over5 <- ''
        if (length(input$nasdaq.symbols) > 5) {
            over5 <- 'You can only compare 5 stocks at a time'
        }
        over5
    })
    
    output$compare.price <- renderDygraph({
        symbols <- input$nasdaq.symbols
        price.point <- input$price.point
        df.price <- data.frame()
        
        if (length(symbols) <= 5) {
            for (s in symbols) {
                tryCatch({
                    tmp.tm <- quantmod::getSymbols(
                        s,
                        src = 'yahoo',
                        from = '2018-01-01',
                        auto.assign = FALSE
                    )
                    tmp.df <-
                        data.frame(Date = index(tmp.tm), coredata(tmp.tm))
                    names(tmp.df) <-
                        c('Date',
                          'Open',
                          'High',
                          'Low',
                          'Close',
                          'Volume',
                          'Adjusted')
                    tmp.price <-
                        dplyr::select(tmp.df, Date, price.point)
                    names(tmp.price) <- c('Date', s)
                    if (nrow(df.price) == 0) {
                        df.price <- tmp.price
                    }
                    else {
                        df.price <- merge(df.price, tmp.price, by = 'Date')
                    }
                },
                error = function(cond) {
                    message(cond)
                },
                warning = function(cond) {
                    message(cond)
                    
                })
            }
        }
        
        if (nrow(df.price) > 0) {
            don.mult <- xts(x = df.price[, -1], order.by = df.price$Date)
            
            dgraph <-
                dygraph(don.mult, main = 'Price Tracker') %>%
                dyAxis('y', label = 'Price') %>%
                dyOptions(
                    axisLineWidth = 2,
                    fillGraph = F,
                    drawGrid = T,
                    strokeWidth = 2,
                    colors = c('red', 'purple', 'steelblue',
                               'seagreen', 'orange')
                ) %>%
                dyRangeSelector()
            
            dgraph
        }
        
    })
    
    output$compare.volume <- renderDygraph({
        symbols <- input$nasdaq.symbols
        price.point <- 'Volume'
        df.volume <- data.frame()
        
        if (length(symbols) <= 5) {
            for (s in symbols) {
                tryCatch({
                    tmp.tm <- quantmod::getSymbols(
                        s,
                        src = 'yahoo',
                        from = '2018-01-01',
                        auto.assign = FALSE
                    )
                    tmp.df <-
                        data.frame(Date = index(tmp.tm), coredata(tmp.tm))
                    names(tmp.df) <-
                        c('Date',
                          'Open',
                          'High',
                          'Low',
                          'Close',
                          'Volume',
                          'Adjusted')
                    tmp.volume <-
                        dplyr::select(tmp.df, Date, price.point)
                    names(tmp.volume) <- c('Date', s)
                    if (nrow(df.volume) == 0) {
                        df.volume <- tmp.volume
                    }
                    else {
                        df.volume <- merge(df.volume, tmp.volume, by = 'Date')
                    }
                },
                error = function(cond) {
                    message(cond)
                },
                warning = function(cond) {
                    message(cond)
                    
                })
            }
        }
        
        if (nrow(df.volume) > 0) {
            don.mult <- xts(x = df.volume[, -1], order.by = df.volume$Date)
            
            dgraph <-
                dygraph(don.mult, main = 'Volume Tracker') %>%
                dyAxis('y', label = 'Volume') %>%
                dyOptions(
                    axisLineWidth = 2,
                    fillGraph = F,
                    drawGrid = T,
                    strokeWidth = 2,
                    colors = c('red', 'purple', 'steelblue',
                               'seagreen', 'orange')
                ) %>%
                dyRangeSelector()
            
            dgraph
        }
        
    })
    
    output$latest.date <- renderText({
        symbols <- input$nasdaq.symbols
        df.data <- data.frame()
        
        if (length(symbols) <= 5) {
            for (s in symbols) {
                tryCatch({
                    tmp.tm <- quantmod::getSymbols(
                        s,
                        src = 'yahoo',
                        from = '2019-01-01',
                        auto.assign = FALSE
                    )
                    tmp.df <-
                        data.frame(Date = index(tmp.tm), coredata(tmp.tm))
                    tmp.s <- c(s)
                    tmp.df <- cbind(tmp.s, tmp.df)
                    names(tmp.df) <-
                        c('Symbol',
                          'Date',
                          'Open',
                          'High',
                          'Low',
                          'Close',
                          'Volume',
                          'Adjusted')
                    df.data <- rbind(df.data, tmp.df)
                },
                error = function(cond) {
                    message(cond)
                },
                warning = function(cond) {
                    message(cond)
                    
                })
            }
        }
        
        if (nrow(df.data) > 0) {
            max.date <- paste('Data as of: ', as.character(as.Date(max(df.data$Date))))
            max.date
        }
    })
    
    output$compare.datatable <- renderDataTable({
        symbols <- input$nasdaq.symbols
        df.data <- data.frame()
        
        if (length(symbols) <= 5) {
            for (s in symbols) {
                tryCatch({
                    tmp.tm <- quantmod::getSymbols(
                        s,
                        src = 'yahoo',
                        from = '2018-01-01',
                        auto.assign = FALSE
                    )
                    tmp.df <-
                        data.frame(Date = index(tmp.tm), coredata(tmp.tm))
                    tmp.s <- c(s)
                    tmp.df <- cbind(tmp.s, tmp.df)
                    names(tmp.df) <-
                        c('Symbol',
                          'Date',
                          'Open',
                          'High',
                          'Low',
                          'Close',
                          'Volume',
                          'Adjusted')
                    df.data <- rbind(df.data, tmp.df)
                },
                error = function(cond) {
                    message(cond)
                },
                warning = function(cond) {
                    message(cond)
                    
                })
            }
        }
        
        if (nrow(df.data) > 0) {
            df.data <- dplyr::filter(df.data, Date == max(Date))
            df.data$Date <- NULL
            dt <- DT::datatable(df.data, options = list(dom = 't'),
                                class = 'cell-border stripe')
            dt
        }
    })
})
