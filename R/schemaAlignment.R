#' @title schemaAlignment
#'
#' @description Identify similarity between 2 datasets and generate a new one that follows as much as possible the schema available in the first dataset while keeping the info available in the second dataset.
#'
#' @details This function will take two  data frames as input and it will try to make the second one \code{y} fit into the first one's \code{x} schema. Ideally \code{y} should be an updated messy version of \code{x}. The function will try to find matches by name first, then by content by running an iterative \code{%in%} operation. These second level operations try to find a match between columns in \code{x} and \code{y}. Only those columns that did not match by name are considered for this second step. If there is a match by content, the column in data frame \code{y} gets renamed according to data frame \code{x}. Then, all those non matching fields are silently dropped and columns in data frame \code{y} are ordered as closely as possible to the original column arrangement in \code{x}. Best if used for small datasets around 10K rows max. Currently there is no option to avoid dropping non-matching columns.This function is verbose as it tries to provide as much visibility as possible to final user as to what is being done under the hood. It will output some debugging flags as well as the name of the columns being dropped from each dataset. Still work in progress.
#'
#' @param x a data frame.
#' @param y a similar data frame that might include additions like new rows, new columns or changed column names and/or positions.
#'
#' @return A new data frame that will include all columns that match 100% by name or by content. It follows the column arrangement of dataset #1 as closely as possible. But it only keeps the content of the second dataset.
#' @examples
#' \dontrun{
#' z <- schemaAlignment(x,y)
#'}
#' @export
#'
#'
schemaAlignment <- function(x,y){

  #Convert to data frame as safety measure
  x <- as.data.frame(x)
  y <- as.data.frame(y)

  #Get column names in variables
  x_names <- names(x)
  y_names <- names(y)


  #Generate dataframe containing column names in Y alongside their expected order
  expectedPosition <- pmatch(y_names,x_names)
  newOrder <- cbind(y_names,expectedPosition)
  newOrder <- as.tibble(newOrder)
  newOrder$expectedPosition <- as.integer(newOrder$expectedPosition)


  #Identify fields in X that could potentially match the NA fields in Y and viceversa
  potentialMatchesinX <- setdiff(1:max(newOrder$expectedPosition, na.rm = T), newOrder$expectedPosition)
  potentialMatchesinY <- which(is.na(newOrder$expectedPosition))


  #Run double loop to iterate through all potential matches in X and Y until finding cases where all X values are available in Y.
  #Then, modify newOrder to be used as a blueprint to rearrange Y
  for (i in 1:length(potentialMatchesinX)){

    for (j in 1:length(potentialMatchesinY)){

      tempResult <- sum(x[ ,potentialMatchesinX[i]] %in% y[ ,potentialMatchesinY[j]])
      print(tempResult)
      print(names(x)[potentialMatchesinX[i]])
      print(names(y)[potentialMatchesinY[j]])
      if (tempResult == length(x[ ,potentialMatchesinX[i]])){ #Use 100% matches as confirmation criteria
        newOrder$y_names[potentialMatchesinY[j]] <-  x_names[potentialMatchesinX[i]]
        newOrder$expectedPosition[potentialMatchesinY[j]] <- i
        print(paste0("Field match found in 1st dataset:",names(x)[potentialMatchesinX[i]]))
        print(paste0("Field renamed in 2nd dataset:", names(y)[potentialMatchesinY[j]]))
      }
    }
  }

  #Print columns that were removed
  removedFields <- which(is.na(newOrder$expectedPosition))
  newOrder <- newOrder[-removedFields, ]
  y <- y[ ,-removedFields]
  print(paste0("Removed fields from 2nd dataset: ", y_names[removedFields]))

  #Rename columns in Y
  colnames(y) <- newOrder$y_names

  #Rearrange columns in Y
  y <- y[ , order(newOrder$expectedPosition)]

  removedFieldsX <- names(x)[!(names(x) %in% names(y))]
  print(paste0("Removed fields from 1st dataset: ", removedFieldsX))

  #Return y
  return(y)

}



