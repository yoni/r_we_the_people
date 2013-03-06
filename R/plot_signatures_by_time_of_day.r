#' Plots the signature counts over time.
#' @param signatures data.frame of the signatures to plot
#' @return ggplot2 plot object
#' @export
#' @examples
#' data(signatures)
#' print(plot_signatures_by_time_of_day(signatures))
plot_signatures_by_time_of_day <- function(signatures) {
  signatures$count <- 1
  title <- sprintf("Signatures by time of day for petition: %s", unique(signatures$petition_id))
  signatures$hour <- format(signatures$created_POSIXct, "%H")
  signatures_by_tod <- ddply(signatures, .(hour), .fun=function(dat) data.frame(count=sum(dat$count)))
  ggplot(signatures_by_tod, aes(x=hour, y=count)) +
    geom_bar() +
    labs(
      x='Time of day',
      y='Signatures',
      title=title
    )
}
