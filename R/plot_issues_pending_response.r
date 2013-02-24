#' Generates a plot of issues pending response over time.
#' @param petitions a data frame of petitions
#' @importFrom ggplot2 ggplot aes geom_point labs
#' @export
#' @examples
#' data(petitions)
#' plot_issues_pending_response(petitions)
plot_issues_pending_response <- function(petitions) {
  issues <- melt_issues(petitions)

  ggplot(
    subset(issues, status=='pending response'),
    aes(
      x=deadline_POSIXct,
      y=issue,
      size=signature.count
    )
  ) +
    geom_point() +
    labs(
      title="Petitions Pending Response",
      x="Deadline",
      y="Issue"
    )
}