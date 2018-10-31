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
source("make_map_base.R")

load("dashboard_data.RData")
load("dashboard_map_data.RData")
load("data_defaults.RData")
# load(".RData")

# This has base_map, data.shape, and sspz_boundary
# save(base_map, data.shape, sspz_boundary, file = "dashboard_map_data.RData")

# Libraries and code used for building
# library(tictoc)

# save(bg_scores, merged_data, merged_data_parcels, biking, file = "dashboard_data.RData")
# subset(bg_scores, select = c(spatial_id, access_score2))
# names(bg_scores)[2] <- "access_score"


# Stuff used for building 
# setwd("C:\\Users\\Max\\Dropbox\\City_Systems\\Scores_Tools\\planning_dashboard\\planning_dashboard_v2\\planning_dashboard_v2")
# setwd("C:\\Users\\Derek\\Documents\\GitHub\\planning_dashboard_v2_updated\\planning_dashboard_v2")

# Creating required variables for use later on 
list_options <- c("All",unique(merged_data$type))

# Define UI 
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
                                      actionButton("go", "Go!"),
                                      downloadButton("downloadData", "Download Scores Table"),
                                      width = 3
                                    ),
                                    mainPanel(
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Map",
                                                           selectInput("type_filter","Amenity Types", list_options),
                                                           fluidRow( 
                                                             column(6, 
                                                                    leafletOutput("basemap")),
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
                                                                    textOutput("new_sspz_avg"),
                                                                    actionButton("update", "Update"))
                                                           )
                                                  ),
                                                  tabPanel("Data Table", dataTableOutput("data_table"))#,
                                                  #tabPanel("Analysis by Type",
                                                  #         selectInput("type_filter","Amenity Types", list_options),
                                                  #         leafletOutput("map_type")))
                                      ) # End main panel
                                    )
                                    
                                  )), # end of scores tabs
                         
                         
                         
                         
                         
                         tabPanel("Airport Way Parcels",
                                  htmlOutput("frame2"))
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  # Maps
  # When using the static base map
  # output$benchmap <- renderLeaflet({base_map})
  
  # tk - could combine both eventReactives relying on go
  
  # The base map 
  base_map_reac <- eventReactive(input$go, {make_map_base()})
  
  output$basemap <- renderLeaflet({base_map_reac()[[1]]})
  
  
  # Scenario maps
  returned_objects <- eventReactive(input$go | input$update, {make_map(type = input$type_filter, df = base_map_reac()[[2]])})
  
  output$new_map <- renderLeaflet({returned_objects()[[1]]})
  output$data_table <- renderDataTable({returned_objects()[[2]]})
  
  # Summary stats
  output$sum_score <- renderText({paste("Benchmark total score:" , round(sum(returned_objects()[[2]]$new_scoreIdeal)))})
  output$new_sum_score <- renderText({paste("Scenario total score:" , round(sum(returned_objects()[[2]]$new_score)))})
  
  output$sspz_score <- renderText({paste("Benchmark promise zone score:",round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal)))})
  output$new_sspz_score <- renderText({paste("Scenario promise zone score:",round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_score)))})
  
  output$bg_avg <- renderText({paste("Benchmark average block group score:" , round(sum(returned_objects()[[2]]$new_scoreIdeal)/nrow(bg_scores)))})
  output$sspz_avg <- renderText({paste("Benchmark promise zone average block groupscore:",round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal)/nrow(sspz_bgs)))})
  
  output$new_bg_avg <- renderText({paste("Scenario average block group score:" , round(sum(returned_objects()[[2]]$new_score)/nrow(bg_scores)))})
  output$new_sspz_avg <- renderText({paste("Scenario promise zone score:",round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_score)/nrow(sspz_bgs)))})
  
  observeEvent(input$type_filter, {
    returned_objects <- eventReactive(input$go | input$update, {make_map(type = input$type_filter, df = base_map_reac()[[2]])})
    output$new_map <- renderLeaflet({returned_objects()[[1]]})
    output$data_table <- renderDataTable({returned_objects()[[2]]})
    output$basemap <- renderLeaflet({make_map_base(input$type_filter)[[1]]})
    output$map_type <- renderLeaflet({make_map_base(input$type_filter)[[1]]})
    # Summary stats
    output$sum_score <- renderText({paste("Benchmark total score:" , round(sum(returned_objects()[[2]]$new_scoreIdeal)))})
    output$new_sum_score <- renderText({paste("Scenario total score:" , round(sum(returned_objects()[[2]]$new_score)))})
    
    output$sspz_score <- renderText({paste("Benchmark promise zone score:",round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal)))})
    output$new_sspz_score <- renderText({paste("Scenario promise zone score:",round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_score)))})
    
    output$bg_avg <- renderText({paste("Benchmark average block group score:" , round(sum(returned_objects()[[2]]$new_scoreIdeal)/nrow(bg_scores)))})
    output$sspz_avg <- renderText({paste("Benchmark promise zone average block groupscore:",round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal)/nrow(sspz_bgs)))})
    
    output$new_bg_avg <- renderText({paste("Scenario average block group score:" , round(sum(returned_objects()[[2]]$new_score)/nrow(bg_scores)))})
    output$new_sspz_avg <- renderText({paste("Scenario promise zone score:",round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_score)/nrow(sspz_bgs)))})
  })
  
  
  
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

