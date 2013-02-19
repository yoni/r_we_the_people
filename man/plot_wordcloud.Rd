\name{plot_wordcloud}
\alias{plot_wordcloud}
\title{Plots a wordcloud for the given petitions.}
\usage{
  plot_wordcloud(petitions, column)
}
\arguments{
  \item{petitions}{a data frame of petitions}

  \item{column}{which column from the petitions to use for
  the word cloud (e.g. 'body', 'title')}
}
\value{
  plot of the wordcloud for all petitions
}
\description{
  Shamelessly taken from example code in
  http://onertipaday.blogspot.com/2011/07/word-cloud-in-r.html
}
\examples{
petitions <- sample_petitions()
print(plot_wordcloud(petitions, 'body'))
}

