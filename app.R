#-------------------------------------
# carregando os pacotes:

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(ggplot2)
library(here)
library(plotly)
library(ggiraph)
library(forcats)
library(readxl)
library(httr)
library(jsonlite)
library(tidyverse)
######## mapas
library(sp) #Caindo em desuso
library(sf) #Esta sendo a melhor opcao entre o sp
library(leaflet) #Talvez n usaremos
library(RColorBrewer)
library(DT)
library(shinyEffects)
library(scales)
library(lubridate)
library(ggthemes)
library(shinyalert)
################

# dando source pra rodar código de extração dos dados assim, quando for fazer
# deploy no shinyapps.io, ele automaticamente faz o download dos dados mais recentes

source("data_wrangling.R", encoding = "UTF-8") 
source("my_ui.R", encoding = "UTF-8")
source("my_server.R", encoding = "UTF-8")


shinyApp(
  ui = ui, 
  server = server
)
