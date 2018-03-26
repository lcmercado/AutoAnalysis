#' Retrieve all reports stored as output of reportsGenerator function and consolidate them.
#'
#' @param dataFrameReports This is the output from reportsGenerator function. It MUST contain a column titled: ReportID
#' @details It is recommended to break the input data frame in chunks of 50 rows in order to prevent memory issues if the reports are too large. Otherwise the server or local computer might run out of memory.
#' @return A data frame containing all the consolidated reports defined by the reportsGenerator function.
#' @examples
#' \dontrun{
#' visitorActivity <- reportsRetriever(dataFrameReports)
#'}
#'
#'

reportsRetriever <- function(dataFrameReports) {
  visitor.activity.list <- lapply(dataFrameReports$ReportID, tryCatch(GetReport))
  visitor.activity.df <-
    as.data.frame(do.call(rbind, visitor.activity.list))

  #Validate report integrity

  if (identical(as.character(unique(visitor.activity.df$datetime)), dataFrameReports$Date)) {
    print("Ok. All reports available")
    return(visitor.activity.df)
  } else {
    print("Some reports may have been missed.")
    missingReportsIndex <- !(as.character(unique(visitor.activity.df$datetime)) %in% dataFrameReports$Date)

    return(visitor.activity.df)
  }

}

