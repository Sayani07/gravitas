source('global_shiny.R', local = TRUE)
# Input Data Tab
tabInput <- tabPanel(
  "Input", fluidPage(
    sidebarPanel(
      # Input csv file
      fileInput("file", "Data file (tsibble as .Rda file)"),
      wellPanel(helpText(HTML("Tsibble provides a data class of tbl_ts to represent tidy temporal data.","It consists of a <b><i>time index</i></b>, <b><i>key</i></b> and other <b><i>measured variables</i></b> in a data-centric format, which makes it easier to work with temporal data.", "To learn more about it, please visit"), a("tsibble", href = "https://tsibble.tidyverts.org/", target = "_blank")
      ))),
    mainPanel(
       fluidRow(
         column(6, h2("Data summary"), verbatimTextOutput("summary"), style = "height:100px"),
         column(6, h2("Data structure"), verbatimTextOutput("str_data"), style = "height:100px")
         # h2("Data structure"), verbatimTextOutput("str_data"), style = "height:100px"),

       )
    )
  )
)


# Create Harmony tab
tabcreate <- tabPanel(
  "Harmony table", fluidPage(
    sidebarPanel(
      # Input csv file
      selectInput("lgran", "lowest temporal unit", gravitas:::lookup_table$granularity, "hour"),
      selectInput("ugran", "highest temporal unit", gravitas:::lookup_table$granularity, "week"),
      numericInput("facet_h", "Maximum number of facets allowed", value = 31, min = 1),
      selectInput("filter_in", "Any other temporal events like Public Holidays/Special Events/Weekends (logical/character vector)", "<select>", multiple = TRUE),
      wellPanel(helpText(HTML(" Combinations of circular granularities which promote the exploratory analysis through visualization are referred to as <b><i>harmonies</i></b> and the ones which impede the analysis are referred to as <b><i>clashes.</i></b> ", "<br>", "<br>", "<br>", "Have a look at the possible harmonies given the lowest and highest temporal unit that you have chosen.","<br>", "<br>", "Is there any Holidays/ Public Events that needs to be checked for Harmonies or Clashes? Add the column which refers to them. Make sure they are logical/categorical" )))
    ),
  mainPanel(
    fluidRow(dataTableOutput("table"))
    )
  )
)

# Create Plot tab

tabplot <- tabPanel(
  "Plot distribution across bivariate granularities", fluidPage(
    sidebarPanel(
      # Input csv file
      selectInput("ycol", "Which univariate time series to plot?", "<select>"),
      selectInput("facet", "Facet Variable", "<select>"),
      selectInput("xcol", "X-axis Variable", "<select>"),
      checkboxInput("flip_axis", "Flipped display", value = FALSE),
      selectInput("plot_type", "Which distribution plot", choices = c("boxplot", "ridge", "violin", "lv",  "decile", "quantile"), selected = "boxplot"),
      textInput('vec1', 'Enter a probability vector (comma delimited) only if quantile plot is chosen', "0.1, 0.5, 0.9"),
      shinyalert::useShinyalert(),  # Set up shinyalert
      actionButton("preview", "Check for warnings/messages"),
      # downloadButton('downloadData', 'Download Data'),
      downloadButton('downloadPlot', 'Download Plot'),
    wellPanel(helpText(HTML("Explore the distribution of the time series variables across bivariate granularities. Choose the distribution plot that best satisfy your contexual needs.", "<br>", "<br>", "Have a look at the messages window to see recommendations on how to improve your choice of granularities.", "<br>","<br>", "<br>", "Does the plot look interesting to you? Go ahead and save it in your workspace.")))),

  mainPanel(
    # conditionalPanel(condition = "output.warnstat == 'Error'",
    #                          verbatimTextOutput("warnmsg")),
    fluidRow(plotOutput("plot1", width = "100%")),
    htmlOutput("code")
  #   aceEditor(
  #     outputId = "ace",
  #     selectionId = "selection",
  #     value = "code",
  #     placeholder = "show nothing"
  #            # htmlOutput("warning_text"))
  #            #"Warning", verbatimTextOutput("warning"))
  #   )
  # )
  )
  )
)


# Granularity table tab
tabgranularity <- tabPanel(
  "Granularity Table", fluidPage(
    # sidebarPanel(
    #   # Input csv file
    #   fileInput("file", "Data file (.csv format)",
    #             accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
    #   )),

      mainPanel(fluidRow(#verbatimTextOutput("clash_txt"),
                         dataTableOutput("grantbl"))
                        )
                )
    )



### Combined tabs
ui <- fluidPage(theme = shinythemes::shinytheme("united"),
                tabsetPanel(
                  type = "tabs",
                    tabInput,
                    tabcreate,
                    tabplot
                    #tabgranularity
  )
)


#
#
# ui <- fluidPage(theme = shinythemes::shinytheme("superhero"),
#                 headerPanel(" Explore probability distributions for bivariate temporal granularities"),
#
#                 sidebarPanel(width = 3,
#                              #tags$style(".well {background-color:[red];}"),
#                              fileInput("file", "Data file (tsibble as .Rda file)"),
#                              selectInput("ycol", "Which univariate time series to plot?", "<select>"),
#                              selectInput("lgran", "lowest temporal unit", gravitas:::lookup_table$granularity, "hour"),
#                              selectInput("ugran", "highest temporal unit", gravitas:::lookup_table$granularity, "week"),
#                              selectInput("facet", "facet Variable", "<select>"), # "<needs update>"),
#                              # search_gran(vic_elec, "hour", "minute")
#                              selectInput("xcol", "X Variable", "<select>"),
#                              selectInput("plot_type", "Which distribution plot", choices = c("boxplot", "ridge", "violin", "lv", "density", "quantile"), selected = "boxplot")
#                 ),
#
#                 mainPanel(
#                   tabsetPanel(
#                     type = "tabs",
#                     tabPanel("Data",
#                              fixedRow(
#                                column(12, "Data summary", verbatimTextOutput("summary")),
#                                column(12, "Data structure",
#                                       verbatimTextOutput("str_data"))
#                              )
#                     ),
#                     # h3("Raw Data"),
#                     #  dataTableOutput("data")),
#                     #,verbatimTextOutput("devMessage3")
#                     # h3("Index of tsibble"),
#                     # textOutput("index"),
#                     # h3("Key of tsibble"),
#                     # textOutput("")),
#                     # # h4("Five point summary"),
#                     # # tableOutput("fivepointsummary")),
#                     # # h4("Raw data"),
#                     # # dataTableOutput("data")),
#                     # # fluidRow(
#                     # #   column(2,
#                     # # tableOutput("summary")
#                     # # ),
#
#                     tabPanel("Harmony Table", tableOutput("table")),
#                     tabPanel("Plot", plotOutput("plot1")),
#                     tabPanel("Granularity Table", dataTableOutput("grantbl"))
#
#                   )
#                 )
# )
