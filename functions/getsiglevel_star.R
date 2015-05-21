 getsiglevel.star <- function (datafield){
  if(datafield> 0.05 ) output <- 'n.s.'
  if(datafield< 0.05 ) output <- '*'
  if(datafield< 0.01 ) output <- '**'
  if(datafield< 0.001) output <- '***'
  return(output)
}