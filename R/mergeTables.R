# Merge all data tables into one.





mergeCompatibleTables <- function(x){
  if(is.character(x)){
  mylist <-   grep(x, ls(pos = 1), value=T)
  mergedDF <- do.call(rbind,  mget(mylist,envir = as.environment(1)))

  return(mergedDF)
  } else {
    stop("Input is not a character string")
  }
}
