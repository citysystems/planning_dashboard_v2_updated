make_map_base <- function(type = "All") {
  
  
  # Used for testing, this does allow data.shape to start with a clean slate each time. Don't really know why I have to do this. 
  load("dashboard_map_data.RData")
  
  
  
  new_scores <- scenario(type, use_new = FALSE)
  
  data.shape@data <- left_join(data.shape@data, new_scores, by = "spatial_id")
  names(data.shape@data)[5] <- "raw_score"
  data.shape@data$bench_ratio <- data.shape$raw_score/sum(ideal_data$score)
  
  
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
