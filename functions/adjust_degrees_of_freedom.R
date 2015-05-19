adjust.degrees.of.freedom <-function (ezANOVAoutput){ 

  for (j in 1:length(ezANOVAoutput[[3]]$GGe) ) {
    match1in3 =match(ezANOVAoutput[[3]]$Effect[j] , ezANOVAoutput[[1]]$Effect)
    ezANOVAoutput[[3]]$GGDFn[j]=ezANOVAoutput[[3]]$GGe[j] * ezANOVAoutput[[1]]$DFn[match1in3]
    ezANOVAoutput[[3]]$GGDFd[j]=ezANOVAoutput[[3]]$GGe[j] * ezANOVAoutput[[1]]$DFd[match1in3]
  }
  return(ezANOVAoutput)
  
}