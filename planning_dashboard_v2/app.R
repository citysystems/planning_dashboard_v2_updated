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
                         #Map of amenities
                         tabPanel("Amenity Locations",
                                  htmlOutput("frame1")),
                         #Table of weights
                         tabPanel("Weightings ",
                                  htmlOutput("weights")), 
                         #Table with proposed parcel land uses
                         tabPanel("Parcel Proposals ",
                                  htmlOutput("parcels_sheet")),
                        
                         #Panel with map and table of accessibility scores
                         tabPanel("Score Calculations",
                                  #Sidebar with go for calculation and a download data button - does this button work?
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
                                                  #Maps - baseline and scenario score maps
                                                  tabPanel("Map",
                                                           #Type filter
                                                           selectInput("type_filter","Amenity Types", list_options),
                                                           
                                                           #Maps
                                                           fluidRow( 
                                                             column(6, 
                                                                    leafletOutput("basemap")),
                                                             column(6,
                                                                    leafletOutput("new_map"))
                                                           ), # End fluid row
                                                           br(),
                                                           
                                                           #Output score values
                                                           fluidRow(
                                                             column(6, 
                                                                    strong('Baseline Analysis'),
                                                                    textOutput("sum_score"),
                                                                    textOutput("sspz_score"),
                                                                    textOutput("bg_avg"),
                                                                    textOutput("sspz_avg")),
                                                             column(6,
                                                                    strong('Scenario Analysis'),
                                                                    textOutput("new_sum_score"),
                                                                    textOutput("new_sspz_score"),
                                                                    textOutput("new_bg_avg"),
                                                                    textOutput("new_sspz_avg"),
                                                                    actionButton("update", "Update"))
                                                           )
                                                  ),
                                                  #Data Table of scores
                                                  tabPanel("Data Table", dataTableOutput("data_table"))#,
                                                  
                                                  #OUTDATED: tabPanel("Analysis by Type",
                                                  #         selectInput("type_filter","Amenity Types", list_options),
                                                  #         leafletOutput("map_type")))
                                      ) # End main panel
                                    )
                                    
                                  )), # end of scores tabs
                         
                         
                         
                         
                         #Map of parcels on airport way
                         tabPanel("Airport Way Parcels",
                                  htmlOutput("frame2"))
))

# Define server logic
server <- function(input, output) {
  
  
  
  # Maps
  # When using the static base map
  # output$benchmap <- renderLeaflet({base_map})
  
  # tk - could combine both eventReactives relying on go
  
  # The base map-create when go or update is pressed
  base_map_reac <- eventReactive(input$go | input$update, {make_map_base(type = input$type_filter)})
  
  output$basemap <- renderLeaflet({base_map_reac()[[1]]})
  
  #returned_objects_OG <- eventReactive(input$go | input$update, {make_map_base(type = input$type_filter)})
  
  
  # Scenario maps
  returned_objects <- eventReactive(input$go | input$update, {make_map(type = input$type_filter, df = base_map_reac()[[2]])})
  
  output$new_map <- renderLeaflet({returned_objects()[[1]]})
  output$data_table <- renderDataTable({returned_objects()[[2]]})
  
  # Summary stats - accessibility scores - normalized and raw sums (citywide and south stockton)
  output$sum_score <- renderText({paste("Citywide accessibility:" , round(sum(returned_objects()[[2]]$new_scoreBenchmark)/sum(returned_objects()[[2]]$new_scoreIdeal),digits = 2), "(raw: ", round(sum(returned_objects()[[2]]$new_scoreBenchmark)), "/ benchmark", round(sum(returned_objects()[[2]]$new_scoreIdeal)),")")})
  output$new_sum_score <- renderText({paste("Citywide accessibility:" , round(sum(returned_objects()[[2]]$new_score)/sum(returned_objects()[[2]]$new_scoreIdeal),digits = 2), "(raw: ", round(sum(returned_objects()[[2]]$new_score)), "/ benchmark", round(sum(returned_objects()[[2]]$new_scoreIdeal)),")")})
  
  output$sspz_score <- renderText({paste("South Stockton accessibility:",round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreBenchmark)/sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal),digits = 2), "(raw: ", round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreBenchmark)), "/ benchmark", round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal)),")")})
  output$new_sspz_score <- renderText({paste("South Stockton accessibility:", round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_score)/sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal),digits = 2), "(raw: ", round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_score)), "/ benchmark", round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal)),")")})
  
  
  #OUTDATED
  #output$bg_avg <- renderText({paste("Baseline average block group score:" , round(sum(returned_objects()[[2]]$new_scoreBenchmark/returned_objects()[[2]]$new_scoreIdeal)/nrow(bg_scores), digits = 2))})
  #output$sspz_avg <- renderText({paste("Baseline promise zone average block groupscore:", round((sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreBenchmark/filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal))/nrow(sspz_bgs),digits = 2))})
  
  #output$new_bg_avg <- renderText({paste("Scenario average block group score:" , round(sum(returned_objects()[[2]]$new_score/returned_objects()[[2]]$new_scoreIdeal)/nrow(bg_scores),digits = 2))})
  #output$new_sspz_avg <- renderText({paste("Scenario promise zone average score:",round((sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_score/filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal))/nrow(sspz_bgs),digits = 2))})
  
  #Recreating the above for when type filter is applied
  observeEvent(input$type_filter, {
    returned_objects <- eventReactive(input$go | input$update, {make_map(type = input$type_filter, df = base_map_reac()[[2]])})
    base_map_reac <- eventReactive(input$go | input$update, {make_map_base(type = input$type_filter)})
    output$new_map <- renderLeaflet({returned_objects()[[1]]})
    output$data_table <- renderDataTable({returned_objects()[[2]]})
    output$basemap <- renderLeaflet({make_map_base(input$type_filter)[[1]]})
    output$map_type <- renderLeaflet({make_map_base(input$type_filter)[[1]]})
    # Summary stats
    output$sum_score <- renderText({paste("Citywide accessibility:" , round(sum(returned_objects()[[2]]$new_scoreBenchmark)/sum(returned_objects()[[2]]$new_scoreIdeal),digits = 2), "(raw: ", round(sum(returned_objects()[[2]]$new_scoreBenchmark)), "/ benchmark", round(sum(returned_objects()[[2]]$new_scoreIdeal)),")")})
    output$new_sum_score <- renderText({paste("Citywide accessibility:" , round(sum(returned_objects()[[2]]$new_score)/sum(returned_objects()[[2]]$new_scoreIdeal),digits = 2), "(raw: ", round(sum(returned_objects()[[2]]$new_score)), "/ benchmark", round(sum(returned_objects()[[2]]$new_scoreIdeal)),")")})
    
    output$sspz_score <- renderText({paste("South Stockton accessibility:",round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreBenchmark)/sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal),digits = 2), "(raw: ", round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreBenchmark)), "/ benchmark", round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal)),")")})
    output$new_sspz_score <- renderText({paste("South Stockton accessibility:", round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_score)/sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal),digits = 2), "(raw: ", round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_score)), "/ benchmark", round(sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal)),")")})
    
    #output$bg_avg <- renderText({paste("Baseline average block group score:" , round(sum(returned_objects()[[2]]$new_scoreBenchmark/returned_objects()[[2]]$new_scoreIdeal)/nrow(bg_scores), digits = 2))})
    #output$sspz_avg <- renderText({paste("Baseline promise zone average block groupscore:", round((sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreBenchmark/filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal))/nrow(sspz_bgs),digits = 2))})
    
    #output$new_bg_avg <- renderText({paste("Scenario average block group score:" , round(sum(returned_objects()[[2]]$new_score/returned_objects()[[2]]$new_scoreIdeal)/nrow(bg_scores),digits = 2))})
    #output$new_sspz_avg <- renderText({paste("Scenario promise zone average score:",round((sum(filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_score/filter(returned_objects()[[2]], spatial_id %in% sspz_bgs$spatial_id)$new_scoreIdeal))/nrow(sspz_bgs),digits = 2))})
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

