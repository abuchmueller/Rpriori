Getsets <- function(items){
  names <- apply(items[[1]], 2, function(set){
    return(paste(names(set)[set], collapse = ','))
  })
  return(data.frame("Items" = names, "Support" = items[[2]]))
}
