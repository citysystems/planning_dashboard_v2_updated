rm(list = ls())
getwd()
setwd("C:/Users/jlund/Documents/GitHub/planning_dashboard_v2_updated-NEWBUTNOTWORKING/planning_dashboard_v2")
load(".RData")
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
type <- "All"
if (type != "All"){
  print("if statement entered")
  # temp_merged <- filter(temp_merged, type == type)
  temp_merged <- temp_merged[temp_merged$type == type,]
  print(nrow(temp_merged))
}


# print(nrow(temp_merged))

#Delete below and use above
scoreCalc <- score_calc(temp_merged$time_biking, temp_merged$time_driving, temp_merged$time_transit, temp_merged$time_walking, temp_merged$abs_good, temp_merged$rank, temp_merged$type)
temp_merged$scores <- scoreCalc[[1]]
temp_merged$scoresAbs <- scoreCalc[[2]]
temp_merged$scoresBike <- scoreCalc[[3]]
temp_merged$scoresDrive <- scoreCalc[[4]]
temp_merged$scoresTransit <- scoreCalc[[5]]
temp_merged$scoresWalk <- scoreCalc[[6]]

# Below here has to change. 
new_scores <- temp_merged %>% group_by(spatial_id) %>% summarise('new_score' = sum(scores, na.rm = TRUE))
new_scoresAbs <- temp_merged %>% group_by(spatial_id) %>% summarise('new_scoreAbs' = sum(scoresAbs, na.rm = TRUE))
new_scoresBike <- temp_merged %>% group_by(spatial_id) %>% summarise('new_scoreBike' = sum(scoresBike, na.rm = TRUE))
new_scoresDrive <- temp_merged %>% group_by(spatial_id) %>% summarise('new_scoreDrive' = sum(scoresDrive, na.rm = TRUE))
new_scoresTransit <- temp_merged %>% group_by(spatial_id) %>% summarise('new_scoreTransit' = sum(scoresTransit, na.rm = TRUE))
new_scoresWalk <- temp_merged %>% group_by(spatial_id) %>% summarise('new_scoreWalk' = sum(scoresWalk, na.rm = TRUE))
scoresFinal <- left_join(new_scores, new_scoresAbs, by='spatial_id')
scoresFinal <- left_join(scoresFinal, new_scoresBike, by='spatial_id')
scoresFinal <- left_join(scoresFinal, new_scoresDrive, by='spatial_id')
scoresFinal <- left_join(scoresFinal, new_scoresTransit, by='spatial_id')
scoresFinal <- left_join(scoresFinal, new_scoresWalk, by='spatial_id')
returns <- list(new_scores, new_scoresAbs, new_scoresBike, new_scoresDrive, new_scoresTransit, new_scoresWalk)
return(returns)


# Used for testing, this does allow data.shape to start with a clean slate each time. 
load("dashboard_map_data.RData")

scenarioCalc <- scenario(type = "All", use_new = TRUE)
scores <- select(scenarioCalc, "new_score", "spatial_id")
scoresAbs <- select(scenarioCalc, "new_scoreAbs", "spatial_id")
scoresBike <- select(scenarioCalc, "new_scoreBike", "spatial_id")
scoresDrive <- select(scenarioCalc, "new_scoreDrive", "spatial_id")
scoresTransit <- select(scenarioCalc, "new_scoreTransit", "spatial_id")
scoresWalk <- select(scenarioCalc, "new_scoreWalk", "spatial_id")

data.shape@data <- left_join(data.shape@data, scores, by = "spatial_id")#  %>% left_join(df)
names(data.shape@data)[5] <- "raw_score"
data.shape@data <- left_join(data.shape@data, scoresAbs, by = "spatial_id")#  %>% left_join(df)
data.shape@data <- left_join(data.shape@data, scoresBike, by = "spatial_id")#  %>% left_join(df)
data.shape@data <- left_join(data.shape@data, scoresDrive, by = "spatial_id")#  %>% left_join(df)
data.shape@data <- left_join(data.shape@data, scoresTransit, by = "spatial_id")#  %>% left_join(df)
data.shape@data <- left_join(data.shape@data, scoresWalk, by = "spatial_id")#  %>% left_join(df)
data.shape@data$bench_ratio <- data.shape$raw_score/sum(ideal_data$score)


#data.shape@data <- left_join(data.shape@data, new_scores, by = "spatial_id") %>% left_join(df)
data.shape@data$score_ratio <- (data.shape@data$new_score/data.shape@data$raw_score)



load("dashboard_map_data.RData")
df <- make_map_base()[[2]]
df <- select(df, 'spatial_id', 'name', 'VALUE0', 'FIPS', 'raw_score')

scenarioCalc <- scenario(type = "All", use_new = TRUE)
scores <- select(scenarioCalc, "new_score", "spatial_id")
scoresAbs <- select(scenarioCalc, "new_scoreAbs", "spatial_id")
scoresBike <- select(scenarioCalc, "new_scoreBike", "spatial_id")
scoresDrive <- select(scenarioCalc, "new_scoreDrive", "spatial_id")
scoresTransit <- select(scenarioCalc, "new_scoreTransit", "spatial_id")
scoresWalk <- select(scenarioCalc, "new_scoreWalk", "spatial_id")

data.shape@data <- left_join(data.shape@data, scores, by = "spatial_id")  %>% left_join(df)
data.shape@data <- left_join(data.shape@data, scoresAbs, by = "spatial_id") %>% left_join(df)
data.shape@data <- left_join(data.shape@data, scoresBike, by = "spatial_id") %>% left_join(df)
data.shape@data <- left_join(data.shape@data, scoresDrive, by = "spatial_id") %>% left_join(df)
data.shape@data <- left_join(data.shape@data, scoresTransit, by = "spatial_id") %>% left_join(df)
data.shape@data <- left_join(data.shape@data, scoresWalk, by = "spatial_id") %>% left_join(df)


ideal_data$ideal_bike[which(ideal_data$type == 'atm')[1]]
which(ideal_data$type == 'atm')[1]



marg_bike    <- exp(time_biking*weights[type , 'marginal_bike'][[1]])
marg_drive   <- exp(time_driving*weights[type , 'marginal_drive'][[1]])
marg_transit <- exp(time_transit*weights[type , 'marginal_transit'][[1]])
marg_walk    <- exp(time_walking*weights[type , 'marginal_walk'][[1]])
ideal_data$ideal_bike[which(ideal_data$type == 'other')[1]]
marg_bikeIdeal    <- exp(ideal_data$ideal_bike[which(ideal_data$type == type)[1]]*weights[type , 'marginal_bike'][[1]])
marg_driveIdeal   <- exp(ideal_data$ideal_drive[which(ideal_data$type == type)[1]]*weights[type , 'marginal_drive'][[1]])
marg_transitIdeal <- exp(ideal_data$ideal_transit[which(ideal_data$type == type)[1]]*weights[type , 'marginal_transit'][[1]])
marg_walkIdeal    <- exp(ideal_data$ideal_walk[which(ideal_data$type == type)[1]]*weights[type , 'marginal_walk'][[1]])


unique(weights$type)
unique(ideal_data$type)
names(ideal_data)


ideal_dummy<-data.frame("other",10,10,10,10,10,10,10)
names(ideal_dummy)<-names(ideal_data)

newdf <- rbind(ideal_dummy, ideal_data)

ideal_dummy<-data.frame("vacant",10,10,10,10,10,10,10)
names(ideal_dummy)<-names(ideal_data)
newdf <- rbind(ideal_dummy, newdf)

ideal_dummy<-data.frame("church",10,10,10,10,10,10,10)
names(ideal_dummy)<-names(ideal_data)
newdf <- rbind(ideal_dummy, newdf)

ideal_dummy<-data.frame("vacant",10,10,10,10,10,10,10)
names(ideal_dummy)<-names(ideal_data)
newdf <- rbind(ideal_dummy, newdf)


load("dashboard_map_data.RData")


scenarioCalcBenchmark <- scenario(type = 'All', use_new = FALSE)
scoresBenchmark <- select(scenarioCalcBenchmark, "new_score", "spatial_id")
names(scoresBenchmark) <- c('new_scoreBenchmark', 'spatial_id')
names(scoresBenchmark)
