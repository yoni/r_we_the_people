#' Plots a trend of signature counts over time.
#' @param petitions data.frame of petitions
plot_signature_count_trends <- function(petitions) {
  ggplot(
    petitions,
    aes(created_POSIXct, signatureCount, color=status)
    ) +
    geom_point() +
    geom_smooth(aes(group=status), method='lm') +
    labs(x='Created', y='Signatures', color='Status')
}
