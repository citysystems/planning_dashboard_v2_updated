score_calc <- function(time_biking, time_driving, time_transit, time_walking,  abs_good, rank, type) {
  
  # I do like the idea of keeping the helper functions in here. Could time it to see what the difference is but the helper functions do keep this one a little cleaner and just generally easier to follow. 
  
  if (! type %in% row.names(weights)) {
    type <- 'other'
  }
  
  # Eq 5
  # it's exp of weight*times
  marg_bike    <- exp(time_biking*weights[type , 'marginal_bike'][[1]])
  marg_drive   <- exp(time_driving*weights[type , 'marginal_drive'][[1]])
  marg_transit <- exp(time_transit*weights[type , 'marginal_transit'][[1]])
  marg_walk    <- exp(time_walking*weights[type , 'marginal_walk'][[1]])
  
  # print(c("margs: ", marg_bike, marg_drive, marg_transit, marg_walk))
  
  # Eq 4
  abs_bike    <- marg_bike*weights[type, 'abs_bike'][[1]]
  abs_drive   <- marg_drive*weights[type, 'abs_drive'][[1]]
  abs_transit <- marg_transit*weights[type, 'abs_transit'][[1]]
  abs_walk    <- marg_walk*weights[type, 'abs_walk'] [[1]] 
  
  # print(c("abs", abs_bike, abs_drive, abs_transit, abs_walk))
  
  # Eq 3
  total_mobility_score <- abs_bike + abs_drive + abs_transit + abs_walk
  # print(total_mobility_score)
  
  # Eq 2, originally in a separate function but I'll move it here to see how it works. 
  marg_good <- marg_good_func(rank, type)
  
  # Eq 1 
  factor_in_rank <- marg_good*total_mobility_score
  
  # Final score, could also put the abs_good function here, may not be a bad idea. 
  score <- abs_good*factor_in_rank
  
  # print(total_mobility_score)
  # print(factor_in_rank)
  return(score)
  
}