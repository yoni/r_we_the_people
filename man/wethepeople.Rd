\docType{package}
\name{wethepeople}
\alias{wethepeople}
\alias{wethepeople-package}
\title{A package for working with the We The People data.}
\description{
  wethepeople is an R package for working with the White
  House's We The People petition data.
}
\examples{
petitions <- load_sample_petitions()
plot_issues_over_time(petitions)
plot_wordcloud(petitions)
}
\author{
  Yoni Ben-Meshulam \email{yoni.bmesh@gmail.com}
}

