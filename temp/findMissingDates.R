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
