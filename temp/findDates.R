findDates <-  function(x) {l
              apply(x,is.Date);
              lapply(x,is.POSIXct);
              lapply(x,is.POSIXlt);
              lapply(x,is.POSIXt);}

#Then I also have to add something like this. To validate those fields that are not explicity set to date but contain dates nonetheless. It has to test MULTIPLE formats, like everything available from Lubridate.
#The output of this function should be a list of date fields, ranges covered, correlations between them, as well as all the fields standarized under thee same format.
## If some fields are not 100% converted to date or detected as date, the list should report a 'likelyhood of date' according to a sample of values.
data <- data.frame(
              Date=c("10/11/2012","10/12/2012"),
              AE=c(1211,100),
              Percent=c(0.03,0.43)
              )

              sapply(data, function(x) !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))))

