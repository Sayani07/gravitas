library(shiny)
library(gravitas)
vic_elec <- tsibbledata::vic_elec


# source('inst/shiny-examples/gravitas_app/ui.R', local = TRUE)
source('ui.R', local = TRUE)
source('global_shiny.R', local = TRUE)
server <- function(input, output, session) {

  # reactive file input
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

  # reactive measured variable

observe({
    updateSelectInput(session,
                      "ycol",
                      choices = fileinput() %>% tsibble::measured_vars()
    )
  })

# reactive lgran variable
  lgran <- reactive({
    if (is.null(input$lgran)) return(NULL)
    isolate({
      input$lgran
    })
  })


  # reactive ugran variable

  ugran <- reactive({
    if (is.null(input$ugran)) return("year")
    isolate({
      input$ugran
    })
  })


  # reactive filter_in variable

  observe({
    updateSelectInput(session,
                      "filter_in",
                      choices = fileinput() %>% tsibble::measured_vars()
                      #select_if(is.logical, is.character) if revision is required for selecting all logical or character vector from teh data
    )
  })


# dynamically update dropdown list for facet - reactive

  observe({
    my_choices <- search_gran(fileinput(), input$ugran, input$lgran)
    updateSelectInput(session,
      "facet",
      choices = my_choices
    )
  })

  # dynamically update dropdown list for x-axis - reactive

  observe({
    my_choices <- search_gran(fileinput(), input$ugran, input$lgran)
    my_choices2 <- my_choices[-match(input$facet, my_choices)]
    updateSelectInput(session,
      "xcol",
      choices = rev(my_choices2)
    )
  })

  # dynamically update dropdown list for x-axis - reactive

  qvec <- reactive({
    as.numeric(unlist(strsplit(input$vec1,",")))
  }
  )

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


  # increment <- reactive({
  #   if (is.null(input$step)) return(NULL)
  #   isolate({
  #     input$step
  #   })
  # })



  output$data <- renderDataTable({
    fileinput()
  })


  output$summary <- renderPrint({
    summary(fileinput())
  })


  output$str_data <- renderPrint({
    str(fileinput())
    # key = tsibble::key(fileinput()),
    # measured_vars = tsibble::measured_vars(fileinput()),
    # interval = tsibble::interval(fileinput()))
  })
  # not suppress warnings
  # storeWarn<- getOption("warn")
  # options(warn = 1)
#
  plot_shiny <-   reactive({
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
  })

  # output$plot1 <- renderPlot({
  #
  #   capture_all_problems(plot_shiny())
  # })

# output for probability vector


  warn_txt = reactive({
    capture_all_problems(
    granplot(
      .data = fileinput(),
      gran1 = input$facet,
      gran2 = input$xcol,
      response = input$ycol,
      plot_type = input$plot_type,
      quantile_prob = qvec()
    )
    )
  })

  output$warning_text <- renderUI({
    #capture_all_problems(plot_shiny())$warning
#
#    warn_txt = capture_all_problems(
#      granplot(
#        .data = fileinput(),
#        gran1 = input$facet,
#        gran2 = input$xcol,
#        response = input$ycol,
#        plot_type = input$plot_type
#      )
#    )
   warn = " "
   warn_txt = warn_txt()
   len_warn_txt <- length(warn_txt$warning)

   for(i in 1:len_warn_txt)
   {
     warn = paste(h3(warn_txt$warning[i]), warn, sep = "<br/>")
   }

    HTML(warn)
  })

  output$plot1 <- renderPlot({
    warn_txt()
  })

      #restore warnings, delayed so plot is completed
      # shinyjs::delay(expr =({
      #   options(warn = storeWarn)
      # }) ,ms = 100)
      #
      # plott
#
#   observeEvent(input$btn, {
#         withCallingHandlers({
#           shinyjs::html(id = "text", html = "")
#           plot_shiny()
#         },
#         warning = function(m) {
#           shinyjs::html(id = "text", html = m$message, add = TRUE)
#         })
#   })

  output$table <- renderDataTable({
    gravitas:::harmony(fileinput(), ugran = ugran() , lgran = lgran(), filter_in =  input$filter_in)
  })


  clash_reason <- reactive(gravitas:::clash_reason(fileinput(), gran1 = input$facet , gran2 = input$xcol))

  output$clash_txt <- renderText({
    clash_reason()[[1]]
  })

  output$grantbl <- renderDataTable({
    clash_reason()[[2]]
  })

  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$file, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = warn_txt()[[1]], device = "png")
    }
  )


  # output$downloadData <- downloadHandler(
  #   filename = function() { paste(input$file, '.csv', sep='') },
  #   content = function(file) {
  #     write.csv(fileinput(), file)
  #   }
  # )

}

shinyApp(ui = ui, server = server)



# granplot(.data = vic_elec,
#          gran1 = "hour_day",
#          gran2 = "hour_day",
#          response = "Demand",
#          plot_type = "boxplot")
