make_map_base <- function(type = "All") {
  
  
  # Used for testing, this does allow data.shape to start with a clean slate each time. Don't really know why I have to do this. 
  load("dashboard_map_data.RData")
  
  
  
  new_scores <- scenario(type)
  
  data.shape@data <- left_join(data.shape@data, new_scores, by = "spatial_id")
  data.shape@data$score_ratio <- (data.shape@data$new_score/data.shape@data$access_score)
  
  
  # Used for testing
  # print(type)
  # print(sum(data.shape@data$access_score))
  # print(sum(new_scores$new_score))
  
  # For testing 
  # View(data.shape@data)
  
  # palette = c("white","darkgreen"),
  
  
  pal <- colorNumeric(
    palette = "RdYlGn",
    domain = data.shape@data$new_score)
  
  new_map <- leaflet(data.shape) %>% 
    addTiles() %>%
    addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.9, smoothFactor = 0.5,
                color=~pal(new_score),weight = 1) %>%
    addLegend("bottomright", pal = pal, values = ~new_score,
              title = "New Scores",
              labFormat = labelFormat(prefix = ""),
              opacity = 1
    )
  
  returned_objects <- list("map" = new_map, "data" = data.shape@data)
  
  return(returned_objects)
  
}
