
# Input Data Tab
tabInput <- tabPanel(
  "Input", fluidPage(
    sidebarPanel(
      # Input csv file
      fileInput("file", "Data file (tsibble as .Rda file)")
      ),
    mainPanel(
    "Data",
       fluidRow(
         column(6, h2("Data summary"), verbatimTextOutput("summary"), style = "height:100px"),
         column(6, h2("Data structure"), verbatimTextOutput("str_data"), style = "height:100px")
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
      selectInput("ugran", "highest temporal unit", gravitas:::lookup_table$granularity, "week")
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
      selectInput("plot_type", "Which distribution plot", choices = c("boxplot", "ridge", "violin", "lv",  "decile", "quantile"), selected = "boxplot"),
      textInput('vec1', 'Enter a probability vector (comma delimited) only if quantile plot is chosen', "0.1, 0.5, 0.9")),
    # shinyjs::useShinyjs(),
    # actionButton("btn","Click me")),

  mainPanel(
    # conditionalPanel(condition = "output.warnstat == 'Error'",
    #                          verbatimTextOutput("warnmsg")),
    fluidRow(fillCol(plotOutput("plot1", height = "100%"),
             htmlOutput("warning_text"))
             #"Warning", verbatimTextOutput("warning"))
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

      mainPanel(fluidRow(verbatimTextOutput("clash_txt"),
                         dataTableOutput("grantbl"))
                )
    )
)



### Combined tabs
ui <- fluidPage(theme = shinythemes::shinytheme("superhero"),
                tabsetPanel(
                  type = "tabs",
                    tabInput,
                    tabcreate,
                    tabplot,
                    tabgranularity
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
