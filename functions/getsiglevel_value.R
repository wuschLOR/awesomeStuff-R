getsiglevel.value.helper <- function (value){
  if(value> 0.05 ) output <- '>0.05'
  if(value< 0.05 ) output <- '<0.05'
  if(value< 0.01 ) output <- '<0.01'
  if(value< 0.001) output <- '<0.001'
  return(output)
}

getsiglevel.value <- function (vector) {
  return(mapply(getsiglevel.value.helper, vector))
}
