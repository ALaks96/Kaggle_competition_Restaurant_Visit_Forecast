% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visit_fct.R
\name{vyear_transf}
\alias{vyear_transf}
\title{vyear_transf}
\usage{
vyear_transf(data)
}
\arguments{
\item{data}{dataframe with categorical variable "visit_date" indicating date of visit}
}
\value{
data with one hot encoded additionnal variables for every year in our data
}
\description{
vyear_transf
}
\examples{
ex <- data.frame(visit_date = c("2016-01-13","2016-04-02","2017-02-11"))
vyear_transf(ex) \%>\% head()

}
