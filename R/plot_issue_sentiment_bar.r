#' Plots issue level sentiment in a simplistic bar chart.
#' @param petitions data.frame of petitions
#' @export
#' @examples
#' data(petitions)
#' plot_issue_sentiment_bar(petitions)
plot_issue_sentiment_bar <- function(petitions) {
  issues <- melt_issues(petitions)
  issue_sentiments <- ddply(issues, .(issue), function(i) data.frame(mean_sentiment=mean(i$sentiment_score)))
  ggplot(issue_sentiments, aes(reorder(issue, mean_sentiment), mean_sentiment, fill=mean_sentiment < 0)) +
    geom_bar() +
    coord_flip() +
    scale_fill_manual(values=c('dark green', 'dark red'), guide='none') +
    labs(y='Average Sentiment', x='Issue') +
    opts(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
      )
}
