% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clickableImageMap.R
\name{annunciator}
\alias{annunciator}
\title{annunciator}
\usage{
annunciator(gtab, row, message, name)
}
\arguments{
\item{gtab}{return value of gtable_replace_grob()}

\item{row}{integer the row number of the annunciator grob in gtab}

\item{message}{character string message to be posted}

\item{name}{character string value of name field in gtab layout matrix}
}
\value{
returns the return value of gtable_add_grob()
}
\description{
post a message in the annunciator grob of gtab
}
\examples{
if(interactive()){
load("data/x_rows.RData")
annunciatorRow<-which(names(x_rows)=="annunciatorRow")
load("data/x_gtab.RData")
annunciator(x_gtab,annunciatorRow,"message","annunciator")
}

}
