#' Supports entity and sentiment analysis using the We the Entities API.
#' See http://wetheentities.herokuapp.com/
#' @importFrom RCurl getURI
#' @importFrom rjson fromJSON
#' @export
#' @examples
#' data(petitions)
#' # Create a client:
#' client <- WeTheEntitiesAPIClient()
#'
#' # Run analysis on a single petition:
#' sample_petitions <- head(petitions, n=3)
#' petition_analyses <- client$analyze_petitions(sample_petitions)
WeTheEntitiesAPIClient <- function() {

  BASE_URL <- 'http://wetheentities.herokuapp.com'

  entity_sentiment <- function(wte_result) {
    ldply(
      wte_result$semantria$entities,
      function(e)
        data.frame(
          entity_title = e$title,
          entity_type = e$entity_type,
          entity_label = e$label,
          entity_confident = e$confident,
          entity_is_about = e$is_about,
          entity_evidence = e$evidence,
          entity_sentiment_score = e$sentiment_score,
          entity_sentiment_polarity = e$sentiment_polarity
        )
    )
  }

  analyze_petitions <- function(petitions) {
    analyses = ddply(
      petitions,
      .(id),
      function(petition) {
        url <- sprintf("http://wetheentities.herokuapp.com/petitions/%s.json", petition$id)
        message(sprintf("Loading analysis from We the Entities (URL: %s)", url))
        wte_result <- fromJSON(getURI(url))
        entity_sentiments <- entity_sentiment(wte_result)
        overall_sentiments <- data.frame(
          sentiment_score = wte_result$semantria$sentiment_score,
          sentiment_polarity = wte_result$semantria$sentiment_polarity
        )
        merge(entity_sentiments, overall_sentiments)
      }
    )
    analyses$sentiment_polarity <- factor(analyses$sentiment_polarity,
      levels=c('negative', 'neutral', 'positive'),
      ordered=TRUE
    )
    merge(petitions, analyses)
  }


  list(
    analyze_petitions=analyze_petitions
    )

}
