\name{plot_entity_sentiment_summary}
\alias{plot_entity_sentiment_summary}
\title{Plots a summary of sentiment for entities.}
\usage{
  plot_entity_sentiment_summary(petitions, n = 10,
    substr_limit = 15,
    entity_types = c("Company", "Person", "Place", "Job Title"))
}
\arguments{
  \item{petitions}{with entity sentiments}

  \item{n}{the number of entities to plot. Will use the top
  entities}

  \item{substr_limit}{length of the entity text to limit
  to}

  \item{entity_types}{limits the entity_type to the given
  values, excluding all other entity types}
}
\value{
  ggplot2 plot of entity sentiments
}
\description{
  Plots a summary of sentiment for entities.
}
\examples{
data(petition_analyses)
plot_entity_sentiment_summary(petition_analyses)
}

