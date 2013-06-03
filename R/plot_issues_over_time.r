#' Generates a plot of issues over time.
#' @param petitions a data frame of petitions
#' @importFrom ggplot2 ggplot aes geom_point labs
#' @export
#' @examples
#' data(petitions)
#' plot_issues_over_time(petitions)
plot_issues_over_time <- function(petitions) {

  issues <- melt_issues(petitions)

  ggplot(
    issues,
    aes(
      x=created_POSIXct,
      y=issue,
      size=signatureCount,
      color=status
    )
  ) +
    geom_point() +
    labs(x="Created", y="Issues")

}
