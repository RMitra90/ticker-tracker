library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dygraphs)
library(TTR)
library(DT)

header <- dashboardHeader(title = 'TICKER TRACKER')

box_height <- "30em"

body <- dashboardBody(
    fluidRow(style = 'height:40vh',
             column(
                 width = 9,
                 box(
                     width = NULL,
                     height = box_height,
                     solidHeader = TRUE,
                     dygraphOutput('compare.price')
                 )
             ),
             column(
                 width = 3,
                 box(
                     width = NULL,
                     height = box_height,
                     solidHeader = TRUE,
                     p(
                         style = 'font-size:16px',
                         class = 'text-muted',
                         'Choose a maximum of 5 NASDAQ Stock Exchange including symbol.'
                     ),
                     awesomeRadio(
                         inputId = 'price.point',
                         label = 'Choose a price point: ',
                         choices = c('Open',
                                     'High',
                                     'Low',
                                     'Close',
                                     'Adjusted')
                     ),
                     verbatimTextOutput('symbols'),
                     selectizeInput(
                         'nasdaq.symbols',
                         'Choose up to 8 stock symbols:',
                         choices = NULL,
                         multiple = TRUE
                     ),
                     span(textOutput('validation'), style = 'color:red')
                 )
             )),
    fluidRow(
        style = 'height:40vh',
        align = 'center',
        column(
            width = 6,
            box(
                width = NULL,
                height = box_height,
                solidHeader = TRUE,
                dygraphOutput('compare.volume')
            )
        ),
        column(
            width = 6,
            box(
                width = NULL,
                height = box_height,
                solidHeader = TRUE,
                h3(style = 'font-weight:bold; font-size:20px', 'Most Recent Data'),
                span(textOutput('latest.date'), style = 'font-size:16px'),
                DT::dataTableOutput('compare.datatable')
            )
        )
    )
)


dashboardPage(header,
              dashboardSidebar(disable = TRUE),
              body)