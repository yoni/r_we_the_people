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

  #' Loads petitions from the API or from a flat JSON file.
  #' @param file optional file to load from instead of hitting the API
  petitions <- function(file=NA) {

    if(is.na(file)) {
      petitions_raw <- fromJSON(getForm(we_the_people_url("petition"), .params=list(key=key)))
    }
    else {
      petitions_raw <- fromJSON(file=file)
    }

    petitions_raw_to_data_frame(petitions_raw)

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
