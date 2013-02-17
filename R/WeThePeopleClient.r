#' We The People API Client, based on RCurl.
#' see RCurl
#' @param key optionsl We the People API Key. All functions except for API calls should work without the key.
#' @return WeThePeople Object with methods for interfacing with the API
#' @importFrom RCurl getForm
#' @export
#' @examples
#' client <- WeThePeopleClient('MY_API_KEY')
WeThePeopleClient <- function(key='') {

  BASE_URL <- "https://petitions.whitehouse.gov/api/v1/"

  we_the_people_url <- function(resource) {
    sprintf("%s/%ss.json", BASE_URL, resource)
  }

  #' Retrieves all petitions up to the given limit.
  get_petitions <- function(limit=NA) {

    result <- NULL
    count <- 0

    repeat {

      fully_qualified_url <- we_the_people_url("petition")
      params <- list(key=key, limit=100, offset=count)

      message("Getting petitions from the We The People API. URL: ", fully_qualified_url, " PARAMS: ", toJSON(params))

      petitions_raw <- fromJSON(getForm(fully_qualified_url, .params=params))
      result_count <- length(petitions_raw$results)

      message("Loaded ", result_count, " petitions")

      if(result_count == 0) {
        break
      }

      petitions.df <- petitions_raw_to_data_frame(petitions_raw)

      if(is.null(result)) {
        result <- petitions.df
      }
      else {
        result <- merge(result, petitions.df, all=TRUE)
      }

      count <- nrow(result)

      if(!is.na(limit) && count >= limit) {
        break
      }

    }

    if(!is.na(limit)) {
      result <- head(result, n=limit)
    }

    result

  }

  #' Loads petitions from the API or from a flat JSON file.
  #' @param file optional file to load from instead of hitting the API
  #' @param limit to limit the number of petitions returned
  petitions <- function(file=NA, limit=NA) {

    if(is.na(file)) {
      get_petitions(limit=limit)
    }
    else {
      petitions_raw_to_data_frame(fromJSON(file=file))
    }

  }

  interface <- list(
    petitions=petitions
  )

  class(interface) <- 'WeThePeople'

  interface

}

#' Loads petition data from a JSON file.
#' @param file the path to the JSON file, assumed to be in the format of a We The People API result
#' @return petitions a data frame of the petitions
#' @importFrom rjson fromJSON
petitions_raw_to_data_frame <- function(petitions_raw) {

  petitions <- ldply(
    petitions_raw$results,
    function(item) {
      as.data.frame(unlist(item, recursive=FALSE), stringsAsFactors=FALSE)
    }
  )

  # Convert to POSIXct fields for convenience.
  for(field in c('created', 'deadline')) {
    petitions[[sprintf('%s_POSIXct', field)]] <- as.POSIXct(petitions[[field]], origin="1970-01-01")
  }

  petitions

}
