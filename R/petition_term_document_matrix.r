#' Calculates the TermDocumentMatrix for petitions.
#' @param petitions data.frame of petitions
#' @param petitions a data frame of petitions
#' @param column which column from the petitions to use for the word cloud (e.g. 'body', 'title')
#' @return TermDocumentMatrix the tdm of the given petition column
#' @importFrom tm Corpus DataframeSource removePunctuation removeWords stopwords
#' @importFrom wordcloud wordcloud
#' @examples
#' data(petitions)
#' petition_body_tdm <- petition_term_document_matrix(petitions, 'body')
#' petition_title_tdm <- petition_term_document_matrix(petitions, 'title')
#'
#' petitions$body_and_title <- paste(petitions$body, petitions$title, sep="\n")
#' petition_body_and_title_tdm <- petition_term_document_matrix(petitions, 'body_and_title')
#' @export
petition_term_document_matrix <- function(petitions, column) {
  petitions_corpus <- Corpus(DataframeSource(data.frame(petitions[[column]])))
  petitions_corpus <- tm_map(petitions_corpus, removePunctuation)
  petitions_corpus <- tm_map(petitions_corpus, tolower)
  petitions_corpus <- tm_map(petitions_corpus, function(x) removeWords(x, stopwords("english")))
  TermDocumentMatrix(petitions_corpus)
}
