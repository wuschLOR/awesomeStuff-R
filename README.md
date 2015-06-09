# awesomeStuff-R
awesome stuff for r
# coole Funktionen

* paired t test  with cohen 
* adjust degree of freedom ezANOVA


# load help functions
file.sources = list.files(pattern="*.R" , path = '/home/wu/git/awesomeStuff-R/functions/' ,full.names = TRUE)
mapply(source,file.sources )