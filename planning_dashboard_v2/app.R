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
# library(tictoc)

# save(bg_scores, merged_data, merged_data_parcels, biking, file = "dashboard_data.RData")
# subset(bg_scores, select = c(spatial_id, access_score2))
# names(bg_scores)[2] <- "baseline_score"


# Stuff used for building 
# setwd("C:\\Users\\Max\\Dropbox\\City_Systems\\Scores_Tools\\planning_dashboard\\planning_dashboard_v2\\planning_dashboard_v2")

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("Planning Dashboard",
                         tabPanel("Amenity Locations",
                                  htmlOutput("frame1")),
                         tabPanel("Weightings ",
                                  htmlOutput("weights")), 
                         tabPanel("Parcel Proposals ",
                                  htmlOutput("parcels_sheet")),
                         
                         
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
                                                                    textOutput("sum_score"),
                                                                    textOutput("sspz_score"),
                                                                    textOutput("bg_avg"),
                                                                    textOutput("sspz_avg")),
                                                             column(6,
                                                                    textOutput("new_sum_score"),
                                                                    textOutput("new_sspz_score"),
                                                                    textOutput("new_bg_avg"),
                                                                    textOutput("new_sspz_avg"))
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
  
  output$sspz_score <- renderText({paste("Benchmark promise zone score:",round(sum(filter(bg_scores, spatial_id %in% sspz_bgs$spatial_id)$baseline_score)))})
  output$new_sspz_score <- renderText({paste("Scenario promise zone score:",round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_score)))})
  
  output$bg_avg <- renderText({paste("Benchmark average block group score:" , round(sum(bg_scores$baseline_score)/nrow(bg_scores)))})
  output$sspz_avg <- renderText({paste("Benchmark promise zone average block groupscore:",round(sum(filter(bg_scores, spatial_id %in% sspz_bgs$spatial_id)$baseline_score)/nrow(sspz_bgs)))})
  
  output$new_bg_avg <- renderText({paste("Scenario average block group score:" , round(sum(returned_objects()[[2]]$new_score)/nrow(bg_scores)))})
  output$new_sspz_avg <- renderText({paste("Scenario promise zone score:",round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_score)/nrow(sspz_bgs)))})
  
  # Rendering the iframes
  output$weights <- renderUI({
    tags$iframe(src = "https://docs.google.com/spreadsheets/d/18_XTChwbtd8dMn_7WDp_qXF6d_VXAhRexgjQTgJq0NY/", seamless=NA, scrolling = "auto", height=600, width=1000)
  })
  
  output$parcels_sheet <- renderUI({
    tags$iframe(src = "https://docs.google.com/spreadsheets/d/1R7dxLoPc-AjvmsdbExF5i2XyfMtZHIG24ziTj-er8Rk/", seamless=NA, scrolling = "auto", height=600, width=800)
  })
  
  output$frame1 <- renderUI({
    tags$iframe(src = "https://www.google.com/maps/d/embed?mid=1Ojkb6ljSIxBkcuqpK0Cwk6S6X69WQp3m", seamless=NA, scrolling = "auto", height=600, width=800)
  })
  
  output$frame2 <- renderUI({
    tags$iframe(src = "https://www.google.com/maps/d/embed?mid=1WqLUetBrETDuibx2gZHaa3k95MowU0jV", seamless=NA, scrolling = "auto", height=600, width=800)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

