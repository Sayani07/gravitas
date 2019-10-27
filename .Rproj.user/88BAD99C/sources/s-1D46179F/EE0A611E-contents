source("global_shiny.R", local = TRUE)
# Input Data Tab
tabInput <- tabPanel(
  "Data summary", fluidPage(
    sidebarPanel(
      # Input rda file

      checkboxInput("default",
                    "Explore package with Victorian electricity demand
                    (Default)",
                    value = TRUE
      ),

      fileInput("file", "Upload your own data file (tsibble as .Rda file)"),
      wellPanel(helpText(HTML(
        "Browse through the package
        with the already loaded data set on",
        "<a href=https://rdrr.io/cran/tsibble
        data/man/vic_elec.html>Victorian
        electricity demand</a>",
        "or load your own dataset (tsibble) in a .Rda file
                              and have a glance of your data before
                              moving ahead with your exploratory journey.",
        "<hr>",
        "<b>Statistical Summary</b>: Provides the five point
summary of all the variables in your data.",
        "<br>",
        "<hr>",
        "<b>Temporal structure</b>: Provides the temporal structure
of the data through a <i>tsibble</i> summary.
It consists of a <i>time index</i>, <i>key</i>
and other <i>measured variables</i>.
The print display gives information on data dimension,
time interval(within third brackets),
keys(if any) and the number of time-based units.",
        "<br>",
        "<br>",
        "<i>Caution</i>: Re-confirm the time interval
        of your data before proceeding with your analysis."
      )))
    ),
    mainPanel(
      fluidRow(
        column(6, h2("Statistical Summary"),
               verbatimTextOutput("summary"),
               style = "height:100px"),
        column(6, h2("Temporal structure"),
               verbatimTextOutput("str_data"),
               style = "height:100px")
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
      selectInput("lowest_unit",
                  "lowest temporal unit",
                  gravitas:::lookup_table$units,
                  "hour"),
      selectInput("highest_unit",
                  "highest temporal unit",
                  gravitas:::lookup_table$units,
                  "week"),
      numericInput("facet_h",
                   "Maximum number of facets allowed",
                   value = 31, min = 1),
      selectInput("filter_in",
                  "Any other temporal events like Public Holidays/Special Events/Weekends (logical/character vector)",
                  "<select>",
                  multiple = TRUE),
      wellPanel(helpText(HTML(" Combinations of circular granularities
                              which promote the exploratory analysis through visualization are referred to as <b><i>harmonies</i></b>
                              and the ones which impede
                              the analysis are referred to as <b><i>clashes.</i></b> ",
                              "<br>", "<br>", "<br>",
                              "Have a look at the possible
                              harmonies given the lowest and highest temporal unit that you have chosen.",
                              "<br>", "<br>",
                              "Make sure the highest temporal
                              unit chosen is higher in temporal order than the lowest temporal unit.
                              Also lowest temporal unit should
                              at least be as large as the tsibble time </i></b> interval</i></b>.",
                              "<br>", "<br>", "<br>",
                              "Is there any Holidays/
                              Public Events that needs to be checked for Harmonies or Clashes?
                              Add the column which refers to them.
                              Make sure they are logical/categorical")))
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
      selectInput(
        "ycol",
        "Which univariate time series to plot?",
        "<select>"
      ),
      selectInput(
        "facet",
        "Facet Variable",
        "<select>"
      ),
      selectInput(
        "xcol",
        "X-axis Variable",
        "<select>"
      ),
      checkboxInput("flip_axis",
        "Flipped display",
        value = FALSE
      ),
      checkboxInput("flip_coord",
        "Flipped coordinates",
        value = FALSE
      ),
      selectInput("plot_type", "Which distribution plot",
        choices = c("boxplot",
                    "ridge",
                    "violin",
                    "lv",
                    "decile",
                    "quantile"),
        selected = "boxplot"
      ),
      textInput(
        "vec1",
        "Enter a probability vector (comma delimited)\n
        only if quantile plot is chosen",
        "0.1, 0.5, 0.9"
      ),
      shinyalert::useShinyalert(), # Set up shinyalert
      actionButton(
        "preview",
        "Check for warnings/messages"
      ),
      # downloadButton('downloadData', 'Download Data'),
      downloadButton("downloadPlot", "Download Plot"),
      wellPanel(helpText(HTML(
        "Explore the distribution of the time series
                            variables across bivariate granularities.
                            Choose the distribution plot that best satisfy
                            your contexual needs.",
        "<br>",
        "<br>",
        "Have a look at the messages window
                            to see recommendations on how to improve
                            your choice of granularities.",
        "<br>", "<br>",
        "<br>", "Does the plot look interesting to you?
                            Go ahead and save it in your workspace."
      )))
    ),

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

    mainPanel(fluidRow( # verbatimTextOutput("clash_txt"),
      dataTableOutput("grantbl")
    ))
  )
)



### Combined tabs
ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  tabsetPanel(
    type = "tabs",
    tabInput,
    tabcreate,
    tabplot
    # tabgranularity
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
