% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{warper_factor_sample_col}
\alias{warper_factor_sample_col}
\title{Function to convert sample columns to factors}
\usage{
warper_factor_sample_col(dataset_exp, col)
}
\arguments{
\item{dataset_exp}{A DatasetExperiment object with sample metadata.}

\item{col}{Column(s) to be converted to factors.}
}
\value{
A DatasetExperiment object with specified columns converted to factors.
}
\description{
This function converts specified columns in the sample metadata to factors.
}
\examples{
warper_factor_sample_col(dataset_exp, col)
}
