#' Plots the signature counts over time.
#' @param signatures
#' @return ggplot2 plot object
plot_signature_count <- function(signatures) {
  signatures$count <- 1
  signatures$cumsum <- cumsum(signatures$count)
  title <- sprintf("Cumulative signatures for petition: %s", unique(signatures$petition_id))
  ggplot(
    signatures,
    aes(
      x=created_POSIXct,
      y=cumsum
    )
  ) +
    geom_smooth() +
    labs(
      x='Date',
      y='Signatures',
      title=title
    )
}
