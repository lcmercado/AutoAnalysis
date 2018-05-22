#' Generate report IDs to be retrieved later
#'
#' @description This function works in tandem with other functions to programatically extract big datasets from Adobe Analytics.
#' @param suite Report suite ID.
#' @param dateBegin Start date in the following format: YYYY-MM-DD.
#' @param dateFinish End date in the following format: YYYY-MM-DD.
#' @param metrics Vector containing up to 30 required metrics IDs.
#' @param elements Vector containing element IDs.
#' @param classification Vector containing classification IDs.
#'@param valueStart Integer value pointing to row to start report with.
#' @return A data frame containing all the report IDs per day. They are required to obtain all trended reports during the specified time frame.
#' @examples
#' \dontrun{
#' ReportsIDs <- reportsGenerator(suite,dateBegin,dateFinish,metrics, elements,classification)
#'}
#' @export


reportsGenerator <- function(suite,
                             dateBegin,
                             dateFinish,
                             metrics,
                             elements,
                             classification,
                             valueStart) {

  #Convert dates to date format.
  #Deduct one from dateBegin to
  #neutralize the initial +1 in the loop.

  dateBegin <-  as.Date(dateBegin, "%Y-%m-%d") - 1
  dateFinish <-  as.Date(dateFinish, "%Y-%m-%d")
  timeRange <- dateFinish - dateBegin

  #Create data frame to store dates and report IDs
  VisitorActivityReports <-
    data.frame(matrix(NA, nrow = timeRange, ncol = 2))
  names(VisitorActivityReports) <- c("Date", "ReportID")

  #Run a loop to retrieve one ReportID for each day in the time period.
  for (i in 1:timeRange) {
    dailyDate <- as.character(dateBegin + i)
    print(i) #Visibility to end user
    print(dailyDate) #Visibility to end user
    VisitorActivityReports[i, 1] <- dailyDate


    VisitorActivityReports[i, 2] <-
      RSiteCatalyst::QueueTrended(
        reportsuite.id = suite,
        date.from = dailyDate,
        date.to = dailyDate,
        metrics = metrics,
        elements = elements,
        classification = classification,
        top = 50000,
        max.attempts = 500,
        start = valueStart,
        enqueueOnly = T
      )
  }
  return(VisitorActivityReports)
}
