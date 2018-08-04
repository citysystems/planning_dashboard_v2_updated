marg_good_func <- function(rank, type) {
  if (type %in% row.names(weights)) {
    return(weights[type,'marginal_good'][[1]]^rank)
  }
  
  return(weights['other', 'marginal_good'][[1]]^rank)
}