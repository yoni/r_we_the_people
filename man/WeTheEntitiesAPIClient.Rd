\name{WeTheEntitiesAPIClient}
\alias{WeTheEntitiesAPIClient}
\title{Supports entity and sentiment analysis using the We the Entities API.
See http://wetheentities.herokuapp.com/}
\usage{
  WeTheEntitiesAPIClient()
}
\description{
  Supports entity and sentiment analysis using the We the
  Entities API. See http://wetheentities.herokuapp.com/
}
\examples{
data(petitions)
# Create a client:
client <- WeTheEntitiesAPIClient()

# Run analysis on a single petition:
sample_petitions <- head(petitions, n=3)
petition_analyses <- client$analyze_petitions(sample_petitions)
}

