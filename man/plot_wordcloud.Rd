\name{plot_wordcloud}
\alias{plot_wordcloud}
\title{Plots a wordcloud for the given petitions.}
\usage{
  plot_wordcloud(petitions, column,
    colors = brewer.pal(8, "Dark2"), scale = c(8, 0.2))
}
\arguments{
  \item{petitions}{a data frame of petitions}

  \item{column}{which column from the petitions to use for
  the word cloud (e.g. 'body', 'title')}

  \item{colors}{the colors to use for the words}

  \item{scale}{the scale to use for the wordcloud plot}
}
\value{
  plot of the wordcloud for all petitions
}
\description{
  Shamelessly taken from example code in
  http://onertipaday.blogspot.com/2011/07/word-cloud-in-r.html
}
\examples{
data(petitions)
one_petition <- head(petitions, n=1)
print(plot_wordcloud(one_petition, 'body'))
}

