#' Plots a wordcloud for the given petitions.
#'
#' Shamelessly taken from example code in http://onertipaday.blogspot.com/2011/07/word-cloud-in-r.html
#'
#' @param petitions
#' @return plot of the wordcloud for all petitions
#' @export
#' @importFrom tm Corpus DataframeSource removePunctuation removeWords stopwords
#' @importFrom wordcloud wordcloud
#' @examples
#' petitions <- load_sample_petitions()
#' print(plot_wordcloud(petitions))
plot_wordcloud <- function(petitions) {
  petitions_corpus <- Corpus(DataframeSource(data.frame(petitions$body)))
  petitions_corpus <- tm_map(petitions_corpus, removePunctuation)
  petitions_corpus <- tm_map(petitions_corpus, tolower)
  petitions_corpus <- tm_map(petitions_corpus, function(x) removeWords(x, stopwords("english")))
  tdm <- TermDocumentMatrix(petitions_corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  pal <- brewer.pal(9, "BuGn")
  wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
}