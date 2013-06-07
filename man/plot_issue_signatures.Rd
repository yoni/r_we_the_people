\name{plot_issue_signatures}
\alias{plot_issue_signatures}
\title{Plot issue signatures using a boxplot.}
\usage{
  plot_issue_signatures(petitions)
}
\arguments{
  \item{petitions}{data.frame of petitions}
}
\value{
  ggplot2 object of the boxplot
}
\description{
  Plot issue signatures using a boxplot.
}
\examples{
data(petitions)
plot_issue_signatures(petitions)
}

