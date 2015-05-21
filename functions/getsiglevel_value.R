getsiglevel.value <- function (datafield){
  if(datafield> 0.05 ) output <- '>0.05'
  if(datafield< 0.05 ) output <- '<0.05'
  if(datafield< 0.01 ) output <- '<0.01'
  if(datafield< 0.001) output <- '<0.001'
  return(output)
}
