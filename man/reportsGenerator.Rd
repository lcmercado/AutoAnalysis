% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportsGenerator.R
\name{reportsGenerator}
\alias{reportsGenerator}
\title{Generate report IDs to be retrieved later}
\usage{
reportsGenerator(suite, dateBegin, dateFinish, metrics, elements,
  classification, valueStart)
}
\arguments{
\item{suite}{Report suite ID.}

\item{dateBegin}{Start date in the following format: YYYY-MM-DD.}

\item{dateFinish}{End date in the following format: YYYY-MM-DD.}

\item{metrics}{Vector containing up to 30 required metrics IDs.}

\item{elements}{Vector containing element IDs.}

\item{classification}{Vector containing classification IDs.}

\item{valueStart}{Integer value pointing to row to start report with.}
}
\value{
A data frame containing all the report IDs per day. They are required to obtain all trended reports during the specified time frame.
}
\description{
This function works in tandem with other functions to programatically extract big datasets from Adobe Analytics.
}
\examples{
\dontrun{
ReportsIDs <- reportsGenerator(suite,dateBegin,dateFinish,metrics, elements,classification)
}
}
