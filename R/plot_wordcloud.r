#' Plots a wordcloud for the given petitions.
#'
#' Shamelessly taken from example code in http://onertipaday.blogspot.com/2011/07/word-cloud-in-r.html
#'
#' @param petitions a data frame of petitions
#' @param column which column from the petitions to use for the word cloud (e.g. 'body', 'title')
#' @return plot of the wordcloud for all petitions
#' @export
#' @importFrom tm Corpus DataframeSource removePunctuation removeWords stopwords
#' @importFrom wordcloud wordcloud
#' @examples
#' data(petitions)
#' print(plot_wordcloud(petitions, 'body'))
plot_wordcloud <- function(petitions, column) {
  tdm <- petition_term_document_matrix(petitions, column)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  pal <- brewer.pal(8, "Dark2")
  wordcloud(
    d$word,
    d$freq,
    scale=c(8,.2),
    min.freq=2,
    max.words=Inf,
    random.order=FALSE,
    rot.per=.15,
    colors=pal
  )
}