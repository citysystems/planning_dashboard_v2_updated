rm(list = ls())
getwd()
setwd("C:/Users/jlund/Documents/GitHub/planning_dashboard_v2_updated/planning_dashboard_v2")
load(".RData")
source('scenario.R')
source('score_calc.R')
source("make_map.R")
source("make_map_base.R")
save.image(file= ".RData")
weights2 <- read.csv('weights.csv')
write.csv('data_defaults.RData', file = "data_defaults.csv")

help("~")

data.shape@data$score_ratio


nrow(sspz_bgs)
write.csv('dashboard_data.RData', file = "dashboard_data.csv")

write.csv('dashboard_map_data.RData', file = "dashboard_map_data.csv")


write.csv(weights, file = "weights2.csv")

write.csv(ideal_data, file='ideal.csv')

write.csv(merged_data, file='merged_data.csv')

write.csv(merged_data_parcels, file='merged_data_parcels.csv')

write.csv(new_scores, file='new_scores.csv')

write.csv(temp_merged, file='temp_merged.csv')
