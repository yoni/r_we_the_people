\name{plot_signature_counts_by_status}
\alias{plot_signature_counts_by_status}
\title{Density plot of signature counts by status, highlighting the sentiment polarity.}
\usage{
  plot_signature_counts_by_status(petitions)
}
\arguments{
  \item{petitions}{data.frame of petitions}
}
\value{
  ggplot2 plot of signatures by status with sentiment
  polarity highlighted
}
\description{
  Density plot of signature counts by status, highlighting
  the sentiment polarity.
}
\examples{
data(petitions)
plot_signature_counts_by_status(petitions)
}

