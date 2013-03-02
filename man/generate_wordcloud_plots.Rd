\name{generate_wordcloud_plots}
\alias{generate_wordcloud_plots}
\title{Generates plots of wordclouds for each petition.}
\usage{
  generate_wordcloud_plots(petitions, path, column)
}
\arguments{
  \item{petitions}{data.frame of petitions, e.g. loaded
  from client$petitions()}

  \item{path}{the path to write the images to}

  \item{column}{the column to use as the input text}
}
\description{
  Generates plots of wordclouds for each petition.
}
\examples{
data(petitions)
one_petition <- head(petitions, n=1)
generate_wordcloud_plots(one_petition, tempdir(), 'body')
}

