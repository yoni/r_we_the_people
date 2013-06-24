#' Plots a boxplot of issues' sentiments.
#' @param petitions data.frame of petitions
#' @export
#' @examples
#' data(petition_analyses)
#' plot_issue_sentiment_boxplot(petition_analyses)
plot_issue_sentiment_boxplot <- function(petitions) {
  issues <- melt_issues(petitions)
  ggplot(issues, aes(reorder(issue, sentiment_score, FUN=median), sentiment_score)) +
    geom_hline(aes(yintercept=mean(sentiment_score), color='red')) +
    geom_hline(yintercept=0, color='black', type='dotted') +
    geom_boxplot() +
    labs(x='Issue', y='Sentiment') +
    coord_flip()
}
