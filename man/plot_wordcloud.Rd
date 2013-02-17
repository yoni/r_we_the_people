\name{plot_wordcloud}
\alias{plot_wordcloud}
\title{Plots a wordcloud for the given petitions.}
\usage{
  plot_wordcloud(petitions)
}
\arguments{
  \item{petitions}{}
}
\value{
  plot of the wordcloud for all petitions
}
\description{
  Shamelessly taken from example code in
  http://onertipaday.blogspot.com/2011/07/word-cloud-in-r.html
}
\examples{
petitions <- load_sample_petitions()
print(plot_wordcloud(petitions))
}

