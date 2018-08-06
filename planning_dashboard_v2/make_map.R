make_map <- function() {
  
  
  # Used for testing 
  # load("dashboard_map_data.RData")
  
  new_scores <- scenario()
  
  data.shape@data <- left_join(data.shape@data, new_scores, by = "spatial_id")
  data.shape@data$score_ratio <- (data.shape@data$new_score/data.shape@data$access_score)
  
  # For testing 
  # View(data.shape@data)
  
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
  
  returned_objects <- list("map" = new_map, "data" = data.shape@data)
  
  return(returned_objects)
  
}