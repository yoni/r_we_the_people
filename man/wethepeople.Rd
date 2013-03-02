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
# Load a sample set of petitions and generate some plots:
data(petitions)
some_petitions <- head(petitions, n=5)
plot_issues_over_time(some_petitions)
plot_wordcloud(some_petitions, 'title')

# Create an API Client
client <- WeThePeopleClient('SOME_API_KEY')
}
\author{
  Yoni Ben-Meshulam \email{yoni@opower.com}
}

