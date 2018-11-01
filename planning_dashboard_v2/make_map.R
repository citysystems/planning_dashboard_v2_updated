make_map <- function(type = "All", df) {
  
  df <- select(df, 'spatial_id', 'name', 'VALUE0', 'FIPS', 'raw_score')
  # Used for testing, this does allow data.shape to start with a clean slate each time. 
  load("dashboard_map_data.RData")
  
  scenarioCalc <- scenario(type = type, use_new = TRUE)
  scenarioCalcBenchmark <- scenario(type = type, use_new = FALSE)
  scores <- select(scenarioCalc, "new_score", "spatial_id")
  scoresBenchmark <- select(scenarioCalcBenchmark, "new_score", "spatial_id")
  names(scoresBenchmark) <- c('new_scoreBenchmark', 'spatial_id')
  scoresAbs <- select(scenarioCalc, "new_scoreAbs", "spatial_id")
  scoresBike <- select(scenarioCalc, "new_scoreBike", "spatial_id")
  scoresDrive <- select(scenarioCalc, "new_scoreDrive", "spatial_id")
  scoresTransit <- select(scenarioCalc, "new_scoreTransit", "spatial_id")
  scoresWalk <- select(scenarioCalc, "new_scoreWalk", "spatial_id")
  scoresIdeal <- select(scenarioCalc, "new_scoreIdeal", "spatial_id")
  
  data.shape@data <- left_join(data.shape@data, scores, by = "spatial_id")  %>% left_join(df)
  data.shape@data <- left_join(data.shape@data, scoresBenchmark, by = "spatial_id")  %>% left_join(df)
  data.shape@data <- left_join(data.shape@data, scoresAbs, by = "spatial_id") %>% left_join(df)
  data.shape@data <- left_join(data.shape@data, scoresBike, by = "spatial_id") %>% left_join(df)
  data.shape@data <- left_join(data.shape@data, scoresDrive, by = "spatial_id") %>% left_join(df)
  data.shape@data <- left_join(data.shape@data, scoresTransit, by = "spatial_id") %>% left_join(df)
  data.shape@data <- left_join(data.shape@data, scoresWalk, by = "spatial_id") %>% left_join(df)
  data.shape@data <- left_join(data.shape@data, scoresIdeal, by = "spatial_id") %>% left_join(df)
  
  #data.shape@data <- left_join(data.shape@data, new_scores, by = "spatial_id") %>% left_join(df)
  data.shape@data$score_ratio <- (data.shape@data$new_score/data.shape@data$new_scoreBenchmark)
  #data.shape@data$score_ratio <- (data.shape@data$new_score/data.shape@data$new_scoreIdeal)
  # For testing 
  # View(data.shape@data)
  
  # 
  # palette = "RdYlGn",
  
  pal <- colorNumeric(
    palette = c("white","darkgreen"),
    domain = data.shape@data$score_ratio)
  
  new_map <- leaflet(data.shape) %>% 
    addTiles() %>%
    addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.9, smoothFactor = 0.5,
                color=~pal(score_ratio),weight = 1) %>%
    addLegend("bottomright", pal = pal, values = ~score_ratio,
              title = "New Scores",
              labFormat = labelFormat(prefix = ""),
              opacity = 1
    )
  
  # View(data.shape@data)
  
  returned_objects <- list("map" = new_map, "data" = data.shape@data)
  
  return(returned_objects)
  
}

