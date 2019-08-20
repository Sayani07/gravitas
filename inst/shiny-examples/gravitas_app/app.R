library(shiny)
library(shinyalert)
library(gravitas)
library(shinyAce)
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
    updateNumericInput(session,
                      "facet_h", value = 31
                      #select_if(is.logical, is.character) if revision is required for selecting all logical or character vector from teh data
    )
  })



  # reactive filter_in variable

  observe({
    updateSelectInput(session,
                      "filter_in",
                      choices =
                        fileinput() %>%
                        as_tibble() %>%
                          select_if(~!is.POSIXlt(.)) %>%
                          select_if(~!is.POSIXct(.)) %>%
                          select_if(~!is.POSIXt(.)) %>%
                          select_if(~!is.Date(.)) %>%
                          select_if(~!is.numeric(.)) %>%
                          names()%>%
                          unique()
                      #  if revision is required for selecting all logical or character vector from teh data
    )
  })


# dynamically update dropdown list for facet - reactive

  observe({
    my_choices <- search_gran(fileinput(), input$ugran, input$lgran)
    updateSelectInput(session,
      "facet",
      choices = c(my_choices, input$filter_in)
    )
  })

  # dynamically update dropdown list for x-axis - reactive

  observe({
    my_choices <- search_gran(fileinput(), input$ugran, input$lgran)
    my_choices2 <- my_choices[-match(input$facet, my_choices)]
    updateSelectInput(session,
      "xcol",
      choices = c(my_choices2, input$filter_in)
    )
  })

  # facet <- reactive({
  #   input$facet
  # })
  #
  #
  # xcol <- reactive({
  #   input$xcol
  # })

  # flip granularities if checkbox is checked

#
#   observeEvent(input$flip_axis, {
#
#     # values <- reactiveValues()
#     # values$xcol <- input$xcol
#     # values$facet <-  input$facet
#
#      updateSelectInput(session,
#     "facet",
#     selected = input$xcol)
#
#      updateSelectInput(session,
#                        "xcol",
#                        selected = input$facet)
#
#
#
#       # temp = input$facet
#       # input$facet = input$xcol
#       # input$xcol = temp
#
#
#   })

  # dynamically update dropdown list for x-axis - reactive

  qvec <- reactive({
    as.numeric(unlist(strsplit(input$vec1,",")))
  }
  )


  output$data <- renderDataTable({
    fileinput()
  })


  output$summary <- renderPrint({
    summary(fileinput())
  })


  output$str_data <- renderPrint({
    fileinput()
    # key = tsibble::key(fileinput()),
    # measured_vars = tsibble::measured_vars(fileinput()),
    # interval = tsibble::interval(fileinput()))
  })
  # not suppress warnings
  # storeWarn<- getOption("warn")
  # options(warn = 1)


  # swap values of facet and x-axis if check box is checked


#
#   plot_shiny <-   reactive({
#     granplot(
#     .data = fileinput(),
#     gran1 = input$facet,
#     gran2 = input$xcol,
#     response = input$ycol,
#     plot_type = input$plot_type
#     # start_lim,
#     # end_lim,
#     # increment
#   )
#   })

  # output$plot1 <- renderPlot({
  #
  #   capture_all_problems(plot_shiny())
  # })

# output for probability vector


  warn_txt = reactive({

    if(input$flip_axis)
      {
      gran1 = input$facet
      gran2 = input$xcol
    }
    else {
      gran1 = input$xcol
      gran2 = input$facet
    }
    capture_all_problems(
     granplot(
      .data = fileinput(),
       gran1 = gran1,
      gran2 = gran2,
      response = input$ycol,
      plot_type = input$plot_type,
      quantile_prob = qvec()
    )
    )
  })

#   output$warning_text <- renderUI({
#     #capture_all_problems(plot_shiny())$warning
# #
# #    warn_txt = capture_all_problems(
# #      granplot(
# #        .data = fileinput(),
# #        gran1 = input$facet,
# #        gran2 = input$xcol,
# #        response = input$ycol,
# #        plot_type = input$plot_type
# #      )
# #    )
#    warn = " "
#    warn_txt = warn_txt()
#    len_warn_txt <- length(warn_txt$warning)
#
#    for(i in 1:len_warn_txt)
#    {
#      warn = paste(h3(warn_txt$warning[i]), warn, sep = "<br/>")
#    }
#
#     HTML(warn)
#   })

  warning_text <- reactive({

    warn = " "
    warn_txt = warn_txt()
    len_warn_txt <- length(warn_txt$warning)

    if(len_warn_txt!=0)
    {
    for(i in 1:len_warn_txt)
    {
      warn = paste(warn_txt$warning[i], warn, sep = '\n')
    }
    }
    else{
      warn = NULL
    }


    warn
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
    gravitas:::harmony(fileinput(), ugran = ugran() , lgran = lgran(), filter_in =  input$filter_in, facet_h = input$facet_h)
  })


  clash_reason <- reactive(gravitas:::clash_reason(fileinput(), gran1 = input$facet , gran2 = input$xcol))

  # show the reason table with 0 observation combination
  clash_txt <- reactive({
    clash_reason()[[1]]
  })

  # show the granularity table with 0 observation combination
  output$grantbl <- renderDataTable({
    clash_reason()[[2]]
  })


  output$code <- renderUI({
    # if(input$flip_axis)
    # {
    #   gran_f = input$facet
    #   gran_x = input$xcol
    # }
    # else {
    #   gran_f = input$xcol
    #   gran_x = input$facet
    # }

    HTML(
       "library(gravitas)",

#
#       expr(
#       gravitas_data <- load(!!input$file$name)),


      expr(
      granplot(
      .data = !!input$file$name,
      gran1 = !!input$facet,
      gran2 = !!input$xcol,
      response = !!input$ycol,
      plot_type = !!input$plot_type,
      quantile_prob = !!qvec())))

  })


  # download the desired plot
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$file, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = warn_txt()[[1]], device = "png")
    }
  )

  observeEvent(input$preview, {
    # Show a modal when the button is pressed
    shinyalert(title = "Check for warnings/messages",
               text = dplyr::if_else(is.null(warning_text()), clash_txt(), warning_text()))
  })





#
# # showing an editor
#   observe({
#     # print all editor content to the R console
#     cat(input$ace, "\n")
#   })
#
#   cdata <- session$clientData
#
#   output$myplot <- renderText({
#     hist(rnorm(input$facet), main = "Generated in renderPlot()")
#   })
#
#   observe({
#     # print only selected editor content to the R console
#     # to access content of `selectionId` use `ace_selection`
#     # i.e., the outputId is prepended to the selectionId for
#     # use with Shiny modules
#
#         .data = fileinput()
#         gran1 = input$facet
#         gran2 = input$xcol
#         response = input$ycol
#         plot_type = input$plot_type
#         quantile_prob = qvec()
#   })



}

shinyApp(ui = ui, server = server)



# granplot(.data = vic_elec,
#          gran1 = "hour_day",
#          gran2 = "hour_day",
#          response = "Demand",
#          plot_type = "boxplot")
