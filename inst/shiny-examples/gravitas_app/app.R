library(shiny)
library(gravitas)
vic_elec <- tsibbledata::vic_elec

ui <- fluidPage(
  headerPanel(" Explore probability distributions for bivariate temporal granularities"),

  sidebarPanel(
    fileInput("file", "Data file (tsibble as .Rda file)"),
    selectInput('lgran', 'lowest temporal unit', gravitas:::lookup_table$granularity, "hour"),
    selectInput('ugran', 'highest temporal unit', gravitas:::lookup_table$granularity, "week"),
    selectInput('facet', 'facet Variable', "<select>"), #"<needs update>"),
    # search_gran(vic_elec, "hour", "minute")
    selectInput('xcol', 'X Variable', "<select>"),
    selectInput('ycol', 'Which univariate time series to plot?', "<select>"),
    radioButtons('plot_type', 'Which distribution plot', choices =    c("boxplot","ridge", "violin", "lv", "density", "percentile", "decile"), selected = "boxplot")

  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output, session) {


  fileinput <- reactive (
    {
      if (is.null(input$file)) return(vic_elec)
      inFile <- isolate({input$file })
      file <- inFile$datapath
      tmp <- new.env()
      load(file, envir = tmp)
      tmp[[ls(tmp)[1]]]%>% tsibble::as_tsibble()
    }
  )


  observe({
    updateSelectInput(session,
                      "ycol",
                      choices = names(fileinput()))
  })

  observe({
    my_choices <- search_gran(fileinput(), input$ugran, input$lgran)
    updateSelectInput(session,
                      "facet",
                      choices = my_choices)
  })
  observe({
    my_choices <- search_gran(fileinput(), input$ugran, input$lgran)
    my_choices2 <- my_choices[-match(input$facet,my_choices)]
    updateSelectInput(session,
                      "xcol",
                      choices = rev(my_choices2))
  })


  # output$plot1 <- renderPlot({
  #   par(mar = c(5.1, 4.1, 0, 1))
  #   plot(selectedData(),
  #        pch = 20, cex = 3)
  # })

  output$plot1 <- renderPlot({
    suppressWarnings(
      granplot(.data = fileinput(),
               gran1 = input$facet,
               gran2 = input$xcol,
               response = input$ycol,
               plot_type = input$plot_type)
    )
  })

}

shinyApp(ui = ui, server = server)




# granplot(.data = vic_elec,
#          gran1 = "hour_day",
#          gran2 = "hour_day",
#          response = "Demand",
#          plot_type = "boxplot")
