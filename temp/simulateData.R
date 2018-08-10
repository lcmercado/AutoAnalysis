#' Simulate data based on randomized component of existing data.
#'
#' @param dateBegin This is the initial date for the data table
#' @param dateEnd This is the end date for the data table
#' @param averageDailyContacts This is the average amount of desired records to be included
#' @param numberContacts This is the amount of contacts (email accounts) to extract from a list of emails available.
#' @param Contacts This is a vector containing all available email accounts from which we want to use some to generate randomized valid data.
#' @param metrics This is a vector containing a string of metrics (columns) to simulate in the data table
#' @param metricSamples This is a list of vectors that include a set of valid values for each metric or a function to generate them. E.g. \code{(stri_rand_strings(100,6,"[A-Z]"))}

#' @return A data frame containing simulated data.
#' @examples
#' \dontrun{
#' simulatedDF <- simulateData(dateBegin,dateEnd,numberContacts,contacts,metrics, metricSamples)
#'}
#'



simulateData <- function(){


}
