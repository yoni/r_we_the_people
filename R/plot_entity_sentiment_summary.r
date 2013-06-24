#' Plots a summary of sentiment for entities.
#' @param petitions with entity sentiments
#' @param n the number of entities to plot. Will use the top entities
#' @param substr_limit length of the entity text to limit to
#' @param entity_types limits the entity_type to the given values, excluding all other entity types
#' @return ggplot2 plot of entity sentiments
#' @export
#' @examples
#' data(petition_analyses)
#' plot_entity_sentiment_summary(petition_analyses)
plot_entity_sentiment_summary <- function(
  petitions,
  n=10,
  substr_limit=15,
  entity_types=c('Company', 'Person', 'Place', 'Job Title')) {

  petitions <- subset(petitions, entity_type %in% entity_types)

  entity_summary <- ddply(
    petitions,
    .(entity_title),
    function(analyses)
      data.frame(
        count=nrow(analyses),
        avg_sentiment=mean(analyses$entity_sentiment_score),
        entity_type=unique(analyses$entity_type)
      )
  )

  dat <- ddply(
    entity_summary,
    .(entity_type),
    function(summaries)
      na.omit(head(summaries[order(entity_summary$count, decreasing=TRUE),], n=n))
  )

  print(dat)

  ggplot(
    dat,
    aes(
      avg_sentiment,
      reorder(entity_title, count),
      size=count
      )
    ) +
    geom_vline(xintercept=0, alpha=0.25) +
    geom_point() +
    labs(x='Entity', y='Sentiment') +
    facet_wrap(~ entity_type, scales='free_y')

}
