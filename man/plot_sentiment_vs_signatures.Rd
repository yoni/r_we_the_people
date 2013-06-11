\name{plot_sentiment_vs_signatures}
\alias{plot_sentiment_vs_signatures}
\title{Plots the sentiment vs. signature count.}
\usage{
  plot_sentiment_vs_signatures(petitions)
}
\arguments{
  \item{petitions}{data.frame of petitions}
}
\value{
  ggplot2 plot of sentiment vs. signatures
}
\description{
  Plots the sentiment vs. signature count.
}
\examples{
data(petitions)
plot_sentiment_vs_signatures(petitions)
}

