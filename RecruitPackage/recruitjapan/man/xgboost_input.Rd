% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xgb_fcts.R
\name{xgboost_input}
\alias{xgboost_input}
\title{xgboost_input}
\usage{
xgboost_input(TEST, TRAIN)
}
\arguments{
\item{TEST}{the test data frame, with a response variable called target}

\item{TRAIN}{the train data frame, with a response variable called target}
}
\value{
a list with dTRAIN (the train matrix, in a xgb_matrix format), dTEST (the test matrix, in a xgb_matrix format)
}
\description{
xgboost_input
}
\examples{
# xgboost_input(test_data_frame,train_data_frame)

}
