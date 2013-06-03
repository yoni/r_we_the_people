#' Plots the statuses according to the creatio time.
#' @param petitions data.frame of petitions
#' @return plot of the status by creation date
plot_status_by_creation <- function(petitions) {
  ggplot(petitions, aes(x=created_POSIXct, y=status, size=signatureCount)) +
    geom_point() +
    labs(x='Created', y='Status')
}
