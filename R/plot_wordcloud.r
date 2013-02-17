#' Plots a wordcloud for the given petitions.
#'
#' Shamelessly taken from example code in http://onertipaday.blogspot.com/2011/07/word-cloud-in-r.html
#'
#' @param petitions
#' @param column which column from the petitions to use for the word cloud (e.g. 'body', 'title')
#' @return plot of the wordcloud for all petitions
#' @export
#' @importFrom tm Corpus DataframeSource removePunctuation removeWords stopwords
#' @importFrom wordcloud wordcloud
#' @examples
#' petitions <- sample_petitions()
#' print(plot_wordcloud(petitions, 'body'))
plot_wordcloud <- function(petitions, column) {
  petitions_corpus <- Corpus(DataframeSource(data.frame(petitions[[column]])))
  petitions_corpus <- tm_map(petitions_corpus, removePunctuation)
  petitions_corpus <- tm_map(petitions_corpus, tolower)
  petitions_corpus <- tm_map(petitions_corpus, function(x) removeWords(x, stopwords("english")))
  tdm <- TermDocumentMatrix(petitions_corpus)
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