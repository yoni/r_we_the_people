#' We The People API Client, based on RCurl.
#' see RCurl
#' @param key optionsl We the People API Key. All functions except for API calls should work without the key.
#' @return WeThePeople Object with methods for interfacing with the API
#' @importFrom RCurl getForm
#' @export
#' @examples
#' client <- WeThePeopleClient('MY_API_KEY')
WeThePeopleClient <- function(key='') {

  BASE_URL <- "https://petitions.whitehouse.gov/api/v1"

  #' Constructs a resource URL based on parent-child relationships defined in the API.
  we_the_people_url <- function(resource, parent=NA, parent_id=NA) {

    has_nested_resource_params <- is.na(c(parent, parent_id))
    if(any(has_nested_resource_params) && !all(has_nested_resource_params))
      stop("You must provide both a parent and an id if using a nested resource.")

    if(is.na(parent)) {
      sprintf("%s/%ss.json", BASE_URL, resource)
    }
    else {
      sprintf("%s/%ss/%s/%ss.json", BASE_URL, parent, parent_id, resource)
    }

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

      petitions.df <- petition_list_to_data_frame(petitions_raw)

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

  #' Retrieves signatures for the given petition_id.
  #' Due to a bug, signatures will not be available until the next release.
  #' See https://github.com/WhiteHouse/hackathon/issues/6
  #' https://petitions.whitehouse.gov/api/v1/petitions/1234/signatures.json?key=bCYUnMF3782k9s8&mock=1
  signatures <- function(petition_id, mock=1) {

    # Override petition_id to be an integer for now. See the bug for details on why.
    petition_id <- 1234

    signatures_vector <- c()
    count <- 0

    repeat {

      fully_qualified_url <- we_the_people_url('signature', parent='petition', parent_id=petition_id)
      params <- list(key=key, limit=100, offset=count, mock=mock)

      message("Getting signatures from the We The People API. URL: ", fully_qualified_url, " PARAMS: ", toJSON(params))

      signatures_raw <- fromJSON(getForm(fully_qualified_url, .params=params))
      signatures_vector_count <- length(signatures_raw$results)

      message("Loaded ", signatures_vector_count, " signatures")

      signatures_vector <- c(signatures_vector, signatures_raw$results)

      count <- nrow(signatures_vector)

      if(signatures_vector_count == 0 || mock == 1) {
        break
      }

    }

    signatures_df <- ldply(signatures_vector, data.frame, stringsAsFactors=FALSE)
    signatures_df$petition_id <- petition_id

    signatures_df <- add_datetime_fields(signatures_df, c('created'))

    signatures_df

  }

  #' Loads petitions from the API or from a flat JSON file.
  #' @param file optional file to load from instead of hitting the API
  #' @param limit to limit the number of petitions returned
  petitions <- function(file=NA, limit=NA) {

    if(is.na(file)) {
      get_petitions(limit=limit)
    }
    else {
      petition_list_to_data_frame(fromJSON(file=file))
    }

  }

  interface <- list(
    petitions=petitions,
    signatures=signatures
  )

  class(interface) <- 'WeThePeople'

  interface

}

#' Converts a nested petition list to a data frame.
#' @param petition_list a nested petition list
#' @return data frame of the entities
#' @importFrom rjson fromJSON
petition_list_to_data_frame <- function(petition_list) {

  petitions <- ldply(
    petition_list$results,
    function(item) {
      as.data.frame(unlist(item, recursive=FALSE), stringsAsFactors=FALSE)
    }
  )

  petitions <- add_datetime_fields(petitions, c('created', 'deadline'))

  petitions

}

add_datetime_fields <- function(entities, fields) {

  for(field in fields) {
    if(field %in% names(entities))
      entities[[sprintf('%s_POSIXct', field)]] <- as.POSIXct(as.numeric(entities[[field]]), origin="1970-01-01")
  }

  entities

}
