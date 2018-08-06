#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sp)
library(leaflet)
library(googlesheets)
library(tidyr)
library(dplyr)

source("rank_fixer.R")
source("score_calc.R")
source("type_splitter.R")
source("weight_adder.R")
source("scenario.R")
source("marg_good_func.R")
source("make_map.R")

load("dashboard_data.RData")
load("dashboard_map_data.RData")

# This has base_map, data.shape, and sspz_boundary

# Libraries and code used for building
library(tictoc)

# save(bg_scores, merged_data, merged_data_parcels, biking, file = "dashboard_data.RData")
# subset(bg_scores, select = c(spatial_id, access_score2))
# names(bg_scores)[2] <- "baseline_score"



# Stuff used for building 
# setwd("C:\\Users\\Max\\Dropbox\\City_Systems\\Scores_Tools\\planning_dashboard\\planning_dashboard_v2\\planning_dashboard_v2")

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("Planning Dashboard",
                         tabPanel("Amenity Locations",
                                  htmlOutput("frame1")),
                         tabPanel("Weightings "),
                         
                         
                         tabPanel("Score Calculations",
                                  sidebarLayout( 
                                    sidebarPanel(
                                      h5("Refer to the following link to create scenarios"),
                                      a("Click here for spreadsheet", href = "https://docs.google.com/spreadsheets/d/1R7dxLoPc-AjvmsdbExF5i2XyfMtZHIG24ziTj-er8Rk/"),
                                      # actionButton("bench", "Calculate Benchmark Scores"),
                                      actionButton("recalc", "Calculate New Scores"),
                                      downloadButton("downloadData", "Download Scores Table"),
                                      width = 3
                                    ),
                                    mainPanel(
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Map",
                                                           fluidRow( 
                                                             column(6, 
                                                                    leafletOutput("benchmap")),
                                                             column(6,
                                                                    leafletOutput("new_map"))
                                                           ), # End fluid row
                                                           br(),
                                                           fluidRow(
                                                             column(6, 
                                                                    textOutput("sum_score")),
                                                             column(6,
                                                                    textOutput("new_sum_score"))
                                                           )
                                                  ),
                                                  tabPanel("Data Table", dataTableOutput("data_table")))
                                    ) # End main panel
                                  )
                                  
                         ), # end of scores tabs
                         
                         
                         
                         
                         
                         tabPanel("Airport Way Parcels",
                                  htmlOutput("frame2"))
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  # Code for maps
  output$benchmap <- renderLeaflet({base_map})
  
  returned_objects <- eventReactive(input$recalc,{make_map()})
  
  output$new_map <- renderLeaflet({returned_objects()[[1]]})
  output$data_table <- renderDataTable({returned_objects()[[2]]})
  
  # Summary stats
  output$sum_score <- renderText({paste("Benchmark total score:" , round(sum(bg_scores$baseline_score)))})
  output$new_sum_score <- renderText({paste("Scenario total score:" , round(sum(returned_objects()[[2]]$new_score)))})
  
  
  
  
  
  output$frame1 <- renderUI({
    tags$iframe(src = "https://www.google.com/maps/d/embed?mid=1Ojkb6ljSIxBkcuqpK0Cwk6S6X69WQp3m", seamless=NA, scrolling = "auto", height=600, width=800)
  })
  
  output$frame2 <- renderUI({
    tags$iframe(src = "https://www.google.com/maps/d/embed?mid=1WqLUetBrETDuibx2gZHaa3k95MowU0jV", seamless=NA, scrolling = "auto", height=600, width=800)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

