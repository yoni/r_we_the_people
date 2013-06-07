#' Plot issue signatures using a boxplot.
#' @param petitions data.frame of petitions
#' @return ggplot2 object of the boxplot
#' @export
#' @examples
#' data(petitions)
#' plot_issue_signatures(petitions)
plot_issue_signatures <- function(petitions) {
  issues <- melt_issues(petitions)
  ggplot(
         issues,
         aes(
             x=issue,
             y=signatureCount
             )
         ) +
        geom_boxplot(
                     outlier.size=2
                     ) +
        geom_jitter(
                    alpha=0.2,
                    size=0.9
                    ) +
        coord_flip()
}
