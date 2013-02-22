\name{petition_term_document_matrix}
\alias{petition_term_document_matrix}
\title{Calculates the TermDocumentMatrix for petitions.}
\usage{
  petition_term_document_matrix(petitions, column)
}
\arguments{
  \item{petitions}{data.frame of petitions}

  \item{petitions}{a data frame of petitions}

  \item{column}{which column from the petitions to use for
  the word cloud (e.g. 'body', 'title')}
}
\value{
  TermDocumentMatrix the tdm of the given petition column
}
\description{
  Calculates the TermDocumentMatrix for petitions.
}
\examples{
data(petitions)
petition_body_tdm <- petition_term_document_matrix(petitions, 'body'))
petition_title_tdm <- petition_term_document_matrix(petitions, 'title'))
}

