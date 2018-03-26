#' @title Bind compatible data tables that have a similar name
#'
#' @description The \code{bindCompatibleTables} function will take a string pattern as input and look for all variables in the Global Environment that include that pattern. Then it will try to \code{rbind} all those variables.
#'
#' @details It's important to have all data frames following the same naming pattern and avoid including that naming pattern in the names of other variables.
#' @param x a string pattern that is also included in all the data frames that have to be binded.

#' @return All binded data frames that include in their name the string pattern given as input.
#' @examples
#' \dontrun{
#' bindedDF <- bindTables("stringPattern")
#'}
#' @export
#'


bindTables <- function(x){
  if(is.character(x)){
  mylist <-   grep(x, ls(pos = 1), value=T)
  bindeddDF <- do.call(rbind,  mget(mylist,envir = as.environment(1)))

  return(bindedDF)
  } else {
    stop("Input is not a character string")
  }
}
