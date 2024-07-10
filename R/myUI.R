############################################################
# R shiny to analyse the performance of the solar cooker   #
# according to the protocol ASAE S580.1 JAN03              #
#                                                          #
#                       PART UI                            #
#                                                          #
# Author: Jonas Meijerink                                  #
# Date: April 2024                                         #
############################################################

############
# Shiny UI #
############

ui <- dashboardPage(
  dashboardHeader(title = "Solar Cooker"),
  skin = "red",

  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload files", tabName = "csv", icon = icon("upload")),
      menuItem("Graphs of teststation data", tabName = "graph", icon = icon("chart-simple")),
      menuItem("Single measure of performance", tabName = "perf", icon = icon("power-off")),
      menuItem("Protocol", tabName = "protocol", icon = icon("book")),
      menuItem("Timeline", tabName = "timeline", icon = icon("timeline")),
      menuItem("Info", tabName = "info", icon = icon("circle-info"))
    ),
    imageOutput("logo_uhasselt")
  ),
  dashboardBody(
    tags$head(tags$style(HTML(".content-wrapper { overflow: auto; }")), tags$style(HTML("/* body */.content-wrapper, .right-side {background-color: #FFFFFF;}"))),
    tabItems(


      # First Tabpanel
      #---------------------------------------------------------------------------
      tabItem(
        tabName = "csv",
        mainPanel(
          strong("This page is build to upload the data of the three testing days. After having uploaded the data, for each day graphs will be made to do data exploration.
                     Also the single measure of performance and survival analysis will be done automaticlay. The results can be found in the tabpanel."),
          tags$hr(),
          p("The download button bellow can be used to download the data compressed to 10min time intervals. This dataset contains as output average measurements of, e.g., the
            ambient temperature (in degrees Celsius) based on repeated measurements within a time period of 10 min.
            As input, the function requires information regarding
            the device used (metadata file) and the dataset obtained from the test station."),
          tags$hr(),
          fluidRow(
            box(
              width = 4, solidHeader = TRUE,
              title = "Test day 1", status = "danger",
              fileInput("file1", "Choose your teststation data file (.CSV)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
              ),
              fileInput("file1meta", "Choose the corresponding metadata file (.CSV)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
              ),
              downloadButton("downloadData1", "Download"),
            ),
            box(
              width = 4, solidHeader = TRUE,
              title = "Test day 2", status = "danger",
              fileInput("file2", "Choose your teststation data file (.CSV)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
              ),
              fileInput("file2meta", "Choose the corresponding metadata file (.CSV)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
              ),
              downloadButton("downloadData2", "Download"),
            ),
            box(
              width = 4, solidHeader = TRUE,
              title = "Test day 3", status = "danger",
              fileInput("file3", "Choose your teststation data file (.CSV)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
              ),
              fileInput("file3meta", "Choose the corresponding metadata file (.CSV)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
              ),
              downloadButton("downloadData3", "Download"),
            )
          ),
          fluidRow(
            box(
              width = 12, collapsed = TRUE,
              title = "Additional information on the teststation data file", status = "danger", solidHeader = TRUE,
              p("The teststation data file should include the following information in the order below:"),
              column(
                12,
                tags$div(
                  id = "info_list",
                  tags$ul(
                    tags$li("Year: year in which the experiment was performed"),
                    tags$li("Month: month in which the experiment was performed"),
                    tags$li("Day: day at which the experiment was performed"),
                    tags$li("Hour: hour of the day at which the observation was collected"),
                    tags$li("Minute: minute at which the observation was collected"),
                    tags$li("Second: second at which the observation was collected"),
                    tags$li("Outdoor_temp: outdoor temperature (in degrees Celsius)"),
                    tags$li("Wind_speed: wind speed"),
                    tags$li("Air_pressure: air pressure"),
                    tags$li("Rel_hum: relative humidity"),
                    tags$li("Temp_pot1: temperature in pot 1 (in degrees Celsius)"),
                    tags$li("Temp_pot2: temperature in pot 2 (in degrees Celsius)"),
                    tags$li("Temp_pot3: temperature in pot 3 (in degrees Celsius)")
                  )
                )
              ),
              collapsible = TRUE
            ),
            box(
              width = 12, collapsed = TRUE,
              title = "Additional information on the metadata file", status = "danger", solidHeader = TRUE,
              p("The metadata file should include the following information in the order below:"),
              column(
                12,
                tags$div(
                  id = "info_list",
                  tags$ul(
                    tags$li("Date (calendar time): TestDate"),
                    tags$li("Volume of water: M"),
                    tags$li("Capacity of water: Cv"),
                    tags$li("Brand of the cooker: Cooker"),
                    tags$li("Type of device: Tdevice"),
                    tags$li("Number of the testing station: TestStation"),
                    tags$li("Number of the testing probe: TempSensor"),
                    tags$li("Type of cooking vessel: Pot"),
                    tags$li("Person performing the experiment: Operator"),
                    tags$li(" Latitude of the testing site: Latitude"),
                    tags$li("Longitude of the testing site: Longitude"),
                    tags$li("Presence or absence of a plastic bag around the pot: PlasticBag"),
                    tags$li("Identification of data from the testing station: Datafile"),
                    tags$li("Picture of the device: CookerPic")
                  )
                )
              ),
              p("Download the template below as example. Change XXXXXX in PEPUNILU2024XXXXXX to the correct number of your datafile."),
              downloadButton("template_meta", "Download"),
              collapsible = TRUE
            ),
            tags$hr()
          )
        ),
      ),

      # Second Tabpanel
      #---------------------------------------------------------------------------
      tabItem(
        tabName = "graph",
        tabsetPanel(
          tabPanel(
            "Graphs day 1",
            mainPanel(
              plotOutput("Plot11"),
              plotOutput("Plot12"),
              plotOutput("Plot13")
            )
          ),
          tabPanel(
            "Graphs day 2",
            mainPanel(
              plotOutput("Plot21"),
              plotOutput("Plot22"),
              plotOutput("Plot23")
            )
          ),
          tabPanel(
            "Graphs day 3",
            mainPanel(
              plotOutput("Plot31"),
              plotOutput("Plot32"),
              plotOutput("Plot33")
            )
          )
        ),
      ),

      # Third Tabpanel
      #---------------------------------------------------------------------------
      tabItem(
        tabName = "perf",
        mainPanel(
          h2("Single measure of performance"),
          strong("A small report will be generated after you uploaded the data for the 3 days."),
          tags$hr(),
          textOutput("output_text1"),
          plotOutput("Plotlinearreg")
        )
      ),

      # Fourth TabPanel with extra subtabs for additional information
      #---------------------------------------------------------------------------

      tabItem(
        tabName = "protocol",
        uiOutput("htmloutput")
      ),

      # Sixth TabPanel with extra subtabs for additional information
      #---------------------------------------------------------------------------
      tabItem(
        tabName = "info",
        h2("Solar cooker Shiny App - Version 1"),
        h3("App Developers"),
        strong("Jonas Meijerink (", a("jonas.meijerink@student.uhasselt.be", href = "mailto:jonas.meijerink@student.uhasselt.be"), ")"), h3("Collaborators"),
        strong("Lowie Van Vyve"),
        h3("Links"),
        a("GitHub Repository", href = "https://github.com/solar-cooker-UHasselt/shiny-data-analysis", target = "_blank")
      ),

      # Seventh TabPanel for the timeline
      #---------------------------------------------------------------------------
      tabItem(
        tabName = "timeline",
        h2("Project timeline"),
        p("Last updated on April 05, 2024."),
        strong("This timeline is under construction and not yet complete!"),
        tags$iframe(src = "timeline.html", style = "width:100%; height:1350px; border:none;")
      )
    )
  )
)
