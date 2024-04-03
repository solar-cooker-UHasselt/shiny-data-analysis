############################################################
# R shiny to analyse the performance of the solar cooker   #
# according to the protocol ASAE S580.1 JAN03              #
#                                                          #
# Author: Jonas Meijerink                                  #
# Date: April 2024                                         #
############################################################

#############
# Libraries #
#############

library(shinydashboard)
library(dplyr)
library(stringr)
library(XML)
library(rvest)
library(lubridate)
library(ggplot2)

######################
# Load UI and server #
######################

source("myUI.R")
source("myserver.R")

#######################
# Run the application #
#######################

shinyApp(ui = ui, server = server)
