% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features.R
\name{goldw_transf}
\alias{goldw_transf}
\title{goldw_transf}
\usage{
goldw_transf(data)
}
\arguments{
\item{data}{dataframe with categorical variable "visit_date" indicating date of visit}
}
\value{
data
}
\description{
goldw_transf
}
\examples{
ex <- data.frame(visit_date = c("2016-01-13","2016-04-02","2017-02-11"))
goldw_transf(ex) \%>\% head()

}
