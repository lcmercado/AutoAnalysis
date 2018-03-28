#Loop used to handle the 13M rows extraction

for (i in 46:length(myseq)) {
  dfName <- paste0("mydf_emailtovisitors",i)
  tempDF <- GetReport(emailToVisitorIDs$ReportID[myseq[i]])
  assign(dfName,tempDF)
}
