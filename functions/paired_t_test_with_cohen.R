library(ez)
library(dplyr)
library(reshape)
library(lsr) # for cohen's d

paired.t.test.with.cohen <- function (dfForComparsion) {

  x=dfForComparsion[,1]
  y=dfForComparsion[,2]
  
  c  <-cohensD(x = x, y = y, method = 'paired')
  
  
  TT<-t.test(x = x, y = y, paired = TRUE ) 
  #werte aus us der t.test liste generieren
  t=TT[[1]] #tvalue
  df=TT[[2]] #df
  p=TT[[3]] #p
  names(t)<-NULL # namen löschen
  names(df)<-NULL # namen löschen
  
#   output.df$t  <- t
#   output.df$df <- df
#   output.df$p  <- p
  
  
  #p output bauen
  sig<-NULL
  if(p> 0.05)  sig ='n.s.'
  if(p< 0.05 ) sig ='*'
  if(p< 0.01 ) sig ='**'
  if(p< 0.001) sig ='***'
  
  # siglevel
  siglevel<-NULL
  if(p> 0.05)  siglevel ='>.05'
  if(p< 0.05 ) siglevel ='<.05'
  if(p< 0.01 ) siglevel ='<.01'
  if(p< 0.001) siglevel ='<.001'
  
  
  
  # init the output dataframe
  output.df<- data.frame(gr1.name    =colnames(dfForComparsion)[1] 
                         , gr1.m     =mean(x)
                         , gr1.sd    =sd(x)
                         , gr2.name  =colnames(dfForComparsion)[2]
                         , gr2.m     =mean(y)
                         , gr2.sd    =sd(y)
                         , df        =df
                         , t         =t
                         , p         =p
                         , sig       =sig
                         , siglevel  =siglevel
                         , cohenD    =c
  )
  
  
#   # output generieren
#   var1 = paste( colnames(dfForComparsion)[1] , '$ M=' , m.col1 , ', SD=' , sd.col1 , ' $' , sep='')
#   var2 = paste( colnames(dfForComparsion)[2] , '$ M=' , m.col2 , ', SD=' , sd.col2 , ' $' , sep='')
#   var3 = paste( '$ t(' , df , ')=', t , ',p<' , sig , ',d=' , d , ' $' , sep='')
#   var4 = paste( '$ t(' , df , ')=', t , ',p>.05'   , ',d=' , d , ' $' , sep='')
#   
#   #check if sig or not
#   ifelse( is.null(sig) , vprint<-var4 , vprint<-var3 )
#   output.df=data.frame( var1=var1 , var2=var2 , vprint=vprint)
#   
#   # noch hübsch benennen
#   colnames(output.df) <- c(colnames(dfForComparsion)[1] , colnames(dfForComparsion)[2] , 't.test')
  
  return(output.df)
}