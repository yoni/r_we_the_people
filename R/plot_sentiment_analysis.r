#' Plots the sentiment together with petitions.
#' @param petitions data.frame of the petitions
#' @export
#' @examples
#' data(petitions)
#' plot_sentiment_analysis(petitions)
plot_sentiment_analysis <- function(petitions) {
  ggplot(
    petitions,
    aes(
      x = deadline_POSIXct,
      y = signatureCount,
      color = sentiment_score,
      size = signatureCount,
      shape = status
      )
    ) +
  geom_point() +
  labs(
    x = 'Deadline',
    y = 'Signature Count',
    color = 'Sentiment',
    size = 'Signature Count',
    shape = 'Status'
  )
}
