% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plots.R
\name{plot_boxplots}
\alias{plot_boxplots}
\title{Plot boxplots for multiple columns}
\usage{
plot_boxplots(data, title = "Boxplot of Columns")
}
\arguments{
\item{data}{The dataset containing the columns to be plotted.}

\item{title}{The title of the plot.}
}
\value{
A ggplot object displaying the boxplots.
}
\description{
This function generates vertical boxplots for multiple columns of a dataset.
}
\examples{
plot_boxplots(data = my_data, title = "Boxplot of Columns")
}
