#' Plots the sentiment density.
#' @param petitions data.frame of the petitions
#' @export
#' @examples
#' data(petitions)
#' plot_sentiment_density(petitions)
plot_sentiment_density <- function(petitions) {
  petitions_issues <- melt_issues(petitions)
  ggplot(
    petitions_issues,
    aes(
      x = sentiment_score
      )
    ) +
  geom_density() +
  labs(
    x = 'Sentiment'
  ) +
  geom_rug() +
  facet_wrap(~ issue)
}
