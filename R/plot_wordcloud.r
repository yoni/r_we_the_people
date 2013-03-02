#' Plots a wordcloud for the given petitions.
#'
#' Shamelessly taken from example code in http://onertipaday.blogspot.com/2011/07/word-cloud-in-r.html
#'
#' @param petitions a data frame of petitions
#' @param column which column from the petitions to use for the word cloud (e.g. 'body', 'title')
#' @param colors the colors to use for the words
#' @param scale the scale to use for the wordcloud plot
#' @return plot of the wordcloud for all petitions
#' @export
#' @importFrom tm Corpus DataframeSource removePunctuation removeWords stopwords
#' @importFrom wordcloud wordcloud
#' @examples
#' data(petitions)
#' one_petition <- head(petitions, n=1)
#' print(plot_wordcloud(one_petition, 'body'))
plot_wordcloud <- function(petitions, column, colors=brewer.pal(8, "Dark2"), scale=c(8, .2)) {
  tdm <- petition_term_document_matrix(petitions, column)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  wordcloud(
    d$word,
    d$freq,
    scale=scale,
    min.freq=2,
    max.words=Inf,
    random.order=FALSE,
    rot.per=.15,
    colors=colors
  )
}
