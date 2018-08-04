rank_fixer <- function(df) {
  # Only need to do this if type is in new type counts
  type <- df$type[1]
  # print(type)
  if (type %in% new_parcel_types) {
    rank_length <- amenity_info[type,]$num
    df$rank <- c(1:rank_length - 1, rep(NA, nrow(df) - rank_length))
  }
  
  return(df)
}