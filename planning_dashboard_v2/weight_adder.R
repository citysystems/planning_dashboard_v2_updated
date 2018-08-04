weight_adder <- function(df, weights) {
  
  df <- df %>% left_join(weights[,c('type','abs_good')], by = "type")
  df$abs_good <-replace_na(df$abs_good, weights['other', 'abs_good'][[1]])
  
  return(df)
}