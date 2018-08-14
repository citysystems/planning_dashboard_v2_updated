scenario <- function(type = "All", use_new = TRUE) {
  
  # When I clear everything, it can't seem to find merged_data_parcels. This is a hacker fix. 
  # Seems to work just fine on shinyapps so I really don't know what's going on. 
  # May keep this here because I'll need merged_data to be refreshed each time after incorporating the select type feature. 
  load("dashboard_data.RData")
  load("data_defaults.RData")
  
  
  # I don't think anything needs to be sent in, it just needs to return the new scores somehow. 
  # tic()
  
  
  
  if (use_new) {
    # Load proposals
    sheet_url <- "https://docs.google.com/spreadsheets/d/1R7dxLoPc-AjvmsdbExF5i2XyfMtZHIG24ziTj-er8Rk/"
    # parcel_proposals <- read_csv("./inputs/parcel_proposals.csv", col_types = cols(APN = col_character(), type = col_character()))
    parcel_proposals <- gs_url(sheet_url) %>% gs_read("Sheet1", range = "A1:E60")
    parcel_proposals$APN <- as.character(parcel_proposals$APN)
    row.names(parcel_proposals) <- parcel_proposals$APN
  } else {
    parcel_proposals <- current_conditions
  }
  
  
  # Load the weights
  weights_url <- "https://docs.google.com/spreadsheets/d/18_XTChwbtd8dMn_7WDp_qXF6d_VXAhRexgjQTgJq0NY/"
  weights <- gs_url(weights_url) %>% gs_read("Sheet1", range = "A1:R18")
  row.names(weights) <- weights$type
  
  parcel_proposals <- parcel_proposals %>% subset(select = -name)
  parcel_proposals <- parcel_proposals %>% gather(num, type, -APN) %>% subset(select = -num)
  parcel_proposals <- parcel_proposals[complete.cases(parcel_proposals$type),]
  
  
  new_parcel_types <- unique(parcel_proposals$type)
  # tk - For now I'm manually addressing types with scores of zero, should be able to come up with a better fix later. 
  new_parcel_types <- new_parcel_types[new_parcel_types != 'vacant']
  
  type_counts <- unlist(lapply(new_parcel_types, function(p_type) return( nrow(parcel_proposals[parcel_proposals$type == p_type,]))))
  
  names(type_counts) <- new_parcel_types
  
  # View(type_counts)
  # Really just for testing. 
  # parcel_proposals %>% group_by(type) %>% summarize(n())
  
  # temp_merged <- merged_data_parcels 
  # This is exactly what I needed. 
  temp_merged <- merged_data_parcels %>% full_join(parcel_proposals, by = c( "parcel" = "APN"))
  temp_merged$rank <- NA
  
  
  # tk - adjust weight adder here to change the weights for merged_data as well
  # Subset doesn't appear to work 
  # merged_data <- subset(merged_data, selecct = -c(abs_good)) 
  
  # Could use this if the above function doesn't work. 
  merged_data <- merged_data[,-which(names(merged_data) == "abs_good")]
  merged_data <- weight_adder(merged_data, weights)
  
  
  
  temp_merged <- weight_adder(temp_merged, weights)
  
  
  
  # Now just need to append the two dataframes and update the ranks where appropriate
  
  # For now, I'll just keep all but could easily trim each subset to only the desired amount of each amenity. 
  
  temp_merged <- subset(temp_merged, select = -parcel) # >
  
  temp_merged <- rbind(merged_data[,names(temp_merged)], temp_merged) # >
  
  temp_merged <- temp_merged[order(temp_merged$spatial_id, temp_merged$type, temp_merged$crow_distance), ] # >
  # tk - eventually needs to change. I think this is fine because I'm using the current definitive lists.
  temp_merged <- temp_merged %>% filter(spatial_id %in% biking$spatial_id) # >
  temp_merged <- temp_merged[complete.cases(temp_merged$crow_distance),] # >
  
  # Duplicate row names seem to be throwing off the boolean indexing
  row.names(temp_merged) <- 1:nrow(temp_merged)
  
  
  # tk - drop the unwanted categories here?
  temp_merged <- temp_merged[temp_merged$type != 'vacant', ]
  
  
  temp_merged <- bind_rows(lapply(split.data.frame(temp_merged, f = temp_merged$spatial_id), type_splitter))
  
  
  
  
  # Removing the NA ranks. 
  temp_merged <- temp_merged[complete.cases(temp_merged$rank),]
  
  
  # print(type)
  # print(nrow(temp_merged))
  
  # temp_merged needs to get filter here
  if (type != "All"){
    print("if statement entered")
    # temp_merged <- filter(temp_merged, type == type)
    temp_merged <- temp_merged[temp_merged$type == type,]
    print(nrow(temp_merged))
  }
  
  # used for making sure that the filtering is actually happening
  
  # print(nrow(temp_merged))
  
  
  temp_merged$scores <- score_calc(temp_merged$time_biking, temp_merged$time_driving, temp_merged$time_transit, temp_merged$time_walking, temp_merged$abs_good, temp_merged$rank, temp_merged$type)
  
  
  # Below here has to change. 
  
  new_scores <- temp_merged %>% group_by(spatial_id) %>% summarise('new_score' = sum(scores, na.rm = TRUE))
  
  return(new_scores)
  
  # bg_scores <- temp_merged %>% group_by(spatial_id) %>% summarise('access_score2' = sum(scores, na.rm = TRUE)) %>% left_join(bg_scores)
  # 
  # bg_scores$diff <- bg_scores$access_score2- bg_scores$access_score
  # bg_scores$diff_prcnt <- bg_scores$access_score2/bg_scores$access_score
  
  
  # toc()
}
