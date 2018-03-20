#' Remove irrelevant columns from data frame
#'
#' @description Remove from data frame those columns that contain either:
#' @description  - only empty values ("")
#' @description  - only zeros (0)
#' @description  - only NAs (NA)
#' @param x This is a data frame.

#' @return The data frame used as an input without empty columns according to rules mentioned above.
#' @examples
#' \dontrun{
#' cleanDataFrame <- RemoveEmptyCols(dataFrame)
#'}
#'
#'

RemoveEmptyCols <- function(x) {

          #Make sure we are dealing with a data frame
          if (!is.data.frame(x)) {
                    stop("Object is not a data frame")
          }

          #Detect, save colname, remove and report all columns that include ONLY empty values
          if (sapply(x, function(x)
                    all(x == "")) == 0) {
                    xEmptyValues <-
                              c(colnames(x[which(sapply(x, function(x)
                                        all(x == "")))]))
                    x <- x[!sapply(x, function(x)
                              all(x == ""))]
                    cat(
                              "The following columns had only EMPTY values and were removed: ",
                              "\n",
                              xEmptyValues,
                              "\n",
                              "\n"
                    )
          }

          #Detect, save colname, remove and report all columns that include ONLY zero values
          if (sapply(x, function(x)
                    all(x == 0)) == 0) {
                    xZeroValues <-
                              c(colnames(x[which(sapply(x, function(x)
                                        all(x == 0)))]))
                    x <- x[!sapply(x, function(x)
                              all(x == 0))]
                    cat(
                              "The following columns had only zero values and were removed: ",
                              "\n",
                              xZeroValues,
                              "\n",
                              "\n"
                    )
          }

          #Detect, save colname, remove and report all columns that include ONLY NAs
          if (sapply(x, function(x)
                    all(is.na(x))) == 0) {
                    xNAs <- c(colnames(x[which(sapply(x, function(x)
                              all(is.na(x))))]))
                    x <- x[!sapply(x, function(x)
                              all(is.na(x)))]
                    cat("The following columns had only NA values and were removed: ",
                        "\n",
                        xNAs,
                        "\n",
                        "\n")
          }

          return(x)
}


