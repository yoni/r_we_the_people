\name{generate_wordcloud_plots}
\alias{generate_wordcloud_plots}
\title{Generates plots of wordclouds for each petition.}
\usage{
  generate_wordcloud_plots(petitions, path, column)
}
\arguments{
  \item{petitions}{data.frame of petitions, e.g. loaded
  from client$petitions()}

  \item{path}{}
}
\description{
  Generates plots of wordclouds for each petition.
}
\examples{
data(petitions)
generate_wordcloud_plots(petitions, tempdir(), 'body')
}

