#' Density plot of signature counts by status, highlighting the sentiment polarity.
#' @param petitions data.frame of petitions
#' @return ggplot2 plot of signatures by status with sentiment polarity highlighted
#' @export
#' @examples
#' data(petitions)
#' plot_signature_counts_by_status(petitions)
plot_signature_counts_by_status <- function(petitions) {
  ggplot(petitions, aes(signatureCount)) +
    geom_density() +
    geom_rug(aes(color=sentiment_polarity)) +
    scale_color_manual(values=c('dark red', 'black', 'dark green')) +
    facet_grid(~status, scales='free') +
    labs(x='Signature Count', y='Density', color='Sentiment')
}
