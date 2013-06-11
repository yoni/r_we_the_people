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

  analyze_petitions <- function(petitions) {
    analyses = ddply(
      petitions,
      .(id),
      function(petition) {
        url <- sprintf("http://wetheentities.herokuapp.com/petitions/%s.json", petition$id)
        message(url)
        wte_result <- fromJSON(getURI(url))
        data.frame(
          sentiment_score = wte_result$semantria$sentiment_score,
          sentiment_polarity = wte_result$semantria$sentiment_polarity
        )
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
