#' Plots the sentiment vs. signature count.
#' @param petitions data.frame of petitions
#' @return ggplot2 plot of sentiment vs. signatures
#' @export
#' @examples
#' data(petitions)
#' plot_sentiment_vs_signatures(petitions)
plot_sentiment_vs_signatures <- function(petitions) {
  ggplot(
    petitions,
    aes(y=sentiment_polarity, x=signatureCount)
    ) +
    geom_jitter() +
    labs(x='Signature Count', y='Sentiment')
}
