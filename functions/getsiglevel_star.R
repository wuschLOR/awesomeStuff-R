getsiglevel.star.helper <- function (value){
  if(value> 0.05 ) output <- 'n.s.'
  if(value< 0.05 ) output <- '*'
  if(value< 0.01 ) output <- '**'
  if(value< 0.001) output <- '***'
  return(output)
}

getsiglevel.star <- function (vector){
  return(mapply(getsiglevel.star.helper, vector))  
}