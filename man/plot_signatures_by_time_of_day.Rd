\name{plot_signatures_by_time_of_day}
\alias{plot_signatures_by_time_of_day}
\title{Plots the signature counts over time.}
\usage{
  plot_signatures_by_time_of_day(signatures)
}
\arguments{
  \item{signatures}{data.frame of the signatures to plot}
}
\value{
  ggplot2 plot object
}
\description{
  Plots the signature counts over time.
}
\examples{
data(signatures)
print(plot_signatures_by_time_of_day(signatures))
}

