score_calc <- function(time_biking, time_driving, time_transit, time_walking,  abs_good, rank, type) {
  
  # Load the weights - THIS IS A FIX
  weights_url <- "https://docs.google.com/spreadsheets/d/18_XTChwbtd8dMn_7WDp_qXF6d_VXAhRexgjQTgJq0NY/"
  weights <- gs_url(weights_url) %>% gs_read("Sheet1", range = "A1:R18")
  row.names(weights) <- weights$type
  
  # I do like the idea of keeping the helper functions in here. Could time it to see what the difference is but the helper functions do keep this one a little cleaner and just generally easier to follow. 
  
  if (! type %in% row.names(weights)) {
    type <- 'other'
  }
  weights$marginal_bike[which(weights$type == "bank")][[1]]
  # Eq 5
  # it's exp of weight*times
  marg_bike    <- exp(time_biking*weights$marginal_bike[which(weights$type == type)][[1]])
  marg_drive   <- exp(time_driving*weights$marginal_drive[which(weights$type == type)][[1]])
  marg_transit <- exp(time_transit*weights$marginal_transit[which(weights$type == type)][[1]])
  marg_walk    <- exp(time_walking*weights$marginal_walk[which(weights$type == type)][[1]])
  
  marg_bikeIdeal    <- exp(newdf$ideal_bike[(which(newdf$type == type)[1])]*weights$marginal_bike[which(weights$type == type)][[1]])
  marg_driveIdeal   <- exp(newdf$ideal_drive[(which(newdf$type == type)[1])]*weights$marginal_drive[which(weights$type == type)][[1]])
  marg_transitIdeal <- exp(newdf$ideal_transit[(which(newdf$type == type)[1])]*weights$marginal_transit[which(weights$type == type)][[1]])
  marg_walkIdeal    <- exp(newdf$ideal_walk[(which(newdf$type == type)[1])]*weights$marginal_walk[which(weights$type == type)][[1]])
  
  
  
  # print(c("margs: ", marg_bike, marg_drive, marg_transit, marg_walk))
  
  # Eq 4
  abs_bike    <- marg_bike*weights[type, 'abs_bike'][[1]]
  abs_drive   <- marg_drive*weights[type, 'abs_drive'][[1]]
  abs_transit <- marg_transit*weights[type, 'abs_transit'][[1]]
  abs_walk    <- marg_walk*weights[type, 'abs_walk'] [[1]] 
  
  abs_bikeIdeal    <- marg_bikeIdeal*weights[type, 'abs_bike'][[1]]
  abs_driveIdeal   <- marg_driveIdeal*weights[type, 'abs_drive'][[1]]
  abs_transitIdeal <- marg_transitIdeal*weights[type, 'abs_transit'][[1]]
  abs_walkIdeal    <- marg_walkIdeal*weights[type, 'abs_walk'] [[1]] 
  
  # print(c("abs", abs_bike, abs_drive, abs_transit, abs_walk))
  
  # Eq 3
  total_mobility_score <- abs_bike + abs_drive + abs_transit + abs_walk
  
  total_mobility_scoreIdeal <- abs_bikeIdeal + abs_driveIdeal + abs_transitIdeal + abs_walkIdeal
  # print(total_mobility_score)
  
  # Eq 2, originally in a separate function but I'll move it here to see how it works. 
  marg_good <- marg_good_func(rank, type)
  
  # Eq 1 
  factor_in_rank <- marg_good*total_mobility_score
  
  factor_in_rankIdeal <- marg_good*total_mobility_scoreIdeal
  
  # Final score, could also put the abs_good function here, may not be a bad idea. 
  score <- abs_good*factor_in_rank
  scoreIdeal <- abs_good*factor_in_rankIdeal
  
  # print(total_mobility_score)
  # print(factor_in_rank)
  #return(score)
  returns <- list(score, total_mobility_score, abs_bike, abs_drive, abs_transit, abs_walk, scoreIdeal)
  return(returns)
  
}