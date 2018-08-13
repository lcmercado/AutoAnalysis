#' Find missing dates between oldest available date and yesterday in a vector.
#'
#' @description This function will take a character vector containing dates and convert it to 'ymd' lubridate format. Then it will compare to an hypothetical vector containing all dates between the oldest date and yesterday.
#' @param suite Character vector containing dates in YYYY-MM-DD format.
#' @return A vector of missing dates in lubridate format.
#' @examples
#' \dontrun{
#' ReportsIDs <- findMissingDates(x)
#'}
#' @export

findMissingDates <- function(x){
  if (is.vector(x) & is.character(x)) {

  } else stop("Not a character vector")

  #Convert to lubridate format
  x <- unique(x)

  #Convert to lubridate format
  x <- ymd(x,tz = "UTC")

  #check if there are missing dates in the report
  minDate <- min(x)
  maxDate <- ymd(today() - 1,tz = "UTC")

  #Calculated expected period length
  periodLength <- maxDate - minDate

  #Generate expected vector/array of dates
  expectedAvailableDates <-  (minDate + days(1:periodLength))


  missingDates <- expectedAvailableDates[!(expectedAvailableDates  %in% x )]

  return(missingDates)

}
