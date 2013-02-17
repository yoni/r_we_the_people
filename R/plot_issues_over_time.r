#' Generates a plot of issues over time.
#' @param petitions a data frame of petitions
#' @export
plot_issues_over_time <- function(petitions) {

  issues <- melt_issues(petitions)

  ggplot(
    issues,
    aes(
      x=created_POSIXct,
      y=issue,
      size=signature.count,
      color=status
    )
  ) +
    geom_point() +
    labs(x="Created", y="Issues")

}