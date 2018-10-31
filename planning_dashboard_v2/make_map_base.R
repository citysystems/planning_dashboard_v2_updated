make_map_base <- function(type = "All") {
  
  
  # Used for testing, this does allow data.shape to start with a clean slate each time. Don't really know why I have to do this. 
  load("dashboard_map_data.RData")
  
  
  scenarioCalc <- scenario(type = type, use_new = TRUE)
  scores <- select(scenarioCalc, "new_score", "spatial_id")
  scoresAbs <- select(scenarioCalc, "new_scoreAbs", "spatial_id")
  scoresBike <- select(scenarioCalc, "new_scoreBike", "spatial_id")
  scoresDrive <- select(scenarioCalc, "new_scoreDrive", "spatial_id")
  scoresTransit <- select(scenarioCalc, "new_scoreTransit", "spatial_id")
  scoresWalk <- select(scenarioCalc, "new_scoreWalk", "spatial_id")
  scoresIdeal <- select(scenarioCalc, "new_scoreIdeal", "spatial_id")
  
  data.shape@data <- left_join(data.shape@data, scores, by = "spatial_id")#  %>% left_join(df)
  names(data.shape@data)[5] <- "raw_score"
  data.shape@data <- left_join(data.shape@data, scoresAbs, by = "spatial_id")#  %>% left_join(df)
  data.shape@data <- left_join(data.shape@data, scoresBike, by = "spatial_id")#  %>% left_join(df)
  data.shape@data <- left_join(data.shape@data, scoresDrive, by = "spatial_id")#  %>% left_join(df)
  data.shape@data <- left_join(data.shape@data, scoresTransit, by = "spatial_id")#  %>% left_join(df)
  data.shape@data <- left_join(data.shape@data, scoresWalk, by = "spatial_id")#  %>% left_join(df)
  data.shape@data <- left_join(data.shape@data, scoresIdeal, by = "spatial_id")#  %>% left_join(df)
  data.shape@data$bench_ratio <- data.shape$raw_score/sum(ideal_data$score) #fix this eventually
  #data.shape@data$bench_ratio <- data.shape$raw_score/sum(data.shape$new_scoreIdeal)
  
  # Used for testing
  # print(type)
  # print(sum(data.shape@data$access_score))
  # print(sum(new_scores$new_score))
  
  # For testing 
  # View(data.shape@data)
  
  # palette = c("white","darkgreen"),
  
  
  pal <- colorNumeric(
    palette = "RdYlGn",
    domain = data.shape@data$bench_ratio)
  
  new_map <- leaflet(data.shape) %>% 
    addTiles() %>%
    addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.9, smoothFactor = 0.5,
                color=~pal(bench_ratio),weight = 1) %>%
    addLegend("bottomright", pal = pal, values = ~bench_ratio,
              title = "Scaled Scores",
              labFormat = labelFormat(prefix = ""),
              opacity = 1
    )
  
  # View(data.shape@data)
  
  returned_objects <- list("map" = new_map, "data" = data.shape@data)
  
  return(returned_objects)
  
}
