library(shiny)
library(gravitas)
vic_elec <- tsibbledata::vic_elec

ui <- fluidPage(theme = shinythemes::shinytheme("superhero"),
  headerPanel(" Explore probability distributions for bivariate temporal granularities"),

  sidebarPanel(width = 3,
    #tags$style(".well {background-color:[red];}"),
    fileInput("file", "Data file (tsibble as .Rda file)"),
    selectInput("ycol", "Which univariate time series to plot?", "<select>"),
    selectInput("lgran", "lowest temporal unit", gravitas:::lookup_table$granularity, "hour"),
    selectInput("ugran", "highest temporal unit", gravitas:::lookup_table$granularity, "week"),
    selectInput("facet", "facet Variable", "<select>"), # "<needs update>"),
    # search_gran(vic_elec, "hour", "minute")
    selectInput("xcol", "X Variable", "<select>"),
    selectInput("plot_type", "Which distribution plot", choices = c("boxplot", "ridge", "violin", "lv", "density", "quantile"), selected = "boxplot")
  ),

  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("Data",
               h3("Tsibble structure"),
                      verbatimTextOutput("str_data"),
               h3("Raw Data"),
                dataTableOutput("data")),
               #,verbatimTextOutput("devMessage3")
               # h3("Index of tsibble"),
               # textOutput("index"),
               # h3("Key of tsibble"),
               # textOutput("")),
               # # h4("Five point summary"),
               # # tableOutput("fivepointsummary")),
               # # h4("Raw data"),
               # # dataTableOutput("data")),
               # # fluidRow(
               # #   column(2,
               # # tableOutput("summary")
               # # ),

      tabPanel("Harmony Table", tableOutput("table")),
      tabPanel("Plot", plotOutput("plot1")),
      tabPanel("Granularity Table", dataTableOutput("grantbl"))

    )
  )
)

server <- function(input, output, session) {
  fileinput <- reactive({
    if (is.null(input$file)) return(vic_elec)
    inFile <- isolate({
      input$file
    })
    file <- inFile$datapath
    tmp <- new.env()
    load(file, envir = tmp)
    tmp[[ls(tmp)[1]]] %>% tsibble::as_tsibble()
  })

observe({
    updateSelectInput(session,
                      "ycol",
                      choices = fileinput() %>% tsibble::measured_vars()
    )
  })


  lgran <- reactive({
    if (is.null(input$lgran)) return(NULL)
    isolate({
      input$lgran
    })
  })


  ugran <- reactive({
    if (is.null(input$ugran)) return("year")
    isolate({
      input$ugran
    })
  })

# dynamically update dropdown list


  observe({
    my_choices <- search_gran(fileinput(), input$ugran, input$lgran)
    updateSelectInput(session,
      "facet",
      choices = my_choices
    )
  })
  observe({
    my_choices <- search_gran(fileinput(), input$ugran, input$lgran)
    my_choices2 <- my_choices[-match(input$facet, my_choices)]
    updateSelectInput(session,
      "xcol",
      choices = rev(my_choices2)
    )
  })


  # output$plot1 <- renderPlot({
  #   par(mar = c(5.1, 4.1, 0, 1))
  #   plot(selectedData(),
  #        pch = 20, cex = 3)
  # })


  # start_lim <- reactive({
  #   isolate({
  #     input$quantile[1]
  #   })
  # })
  #
  #
  # end_lim <- reactive({
  #   isolate({
  #     input$quantile[2]
  #   })
  # })


  increment <- reactive({
    if (is.null(input$step)) return(NULL)
    isolate({
      input$step
    })
  })



  output$data <- renderDataTable({
    fileinput()
  })


  # output$fivepointsummary <- renderDataTable({
  #   summary(fileinput())
  # })


  output$str_data <- renderPrint({
    str(fileinput())
    # key = tsibble::key(fileinput()),
    # measured_vars = tsibble::measured_vars(fileinput()),
    # interval = tsibble::interval(fileinput()))
  })

  output$plot1 <- renderPlot({
    suppressWarnings(
      granplot(
        .data = fileinput(),
        gran1 = input$facet,
        gran2 = input$xcol,
        response = input$ycol,
        plot_type = input$plot_type
        # start_lim,
        # end_lim,
        # increment
      )
    )
  })

  output$table <- renderTable({
    gravitas:::harmony(fileinput(), ugran = ugran() , lgran = lgran())
  })

  output$grantbl <- renderDataTable({
    gravitas:::gran_tbl(fileinput(), gran1 = input$facet , gran2 = input$xcol)
  })

}

shinyApp(ui = ui, server = server)




# granplot(.data = vic_elec,
#          gran1 = "hour_day",
#          gran2 = "hour_day",
#          response = "Demand",
#          plot_type = "boxplot")
