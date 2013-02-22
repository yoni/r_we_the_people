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
      sprintf("%s/%s.json", BASE_URL, resource)
    }
    else {
      sprintf("%s/%s/%s/%s.json", BASE_URL, parent, parent_id, resource)
    }

  }

  #' Retrieves all petitions up to the given limit.
  #' @param resource the name of the resource to get from the We The People API. e.g. petitions, users
  #' @param limit the maximum number of values to get. e.g. 10, 1000. NA returns all values.
  #' @return data.frame of the resource values
  get_resource <- function(resource, limit=NA, parent_id=NA, batch_sizes=list(signatures=1000, users=100, petitions=100)) {

    result <- NULL
    count <- 0

    repeat {

      fully_qualified_url <- switch(
        resource,
        petitions = we_the_people_url(resource),
        users = we_the_people_url(resource),
        signatures = we_the_people_url('signatures', parent='petitions', parent_id=parent_id)
      )

      params <- list(key=key, limit=batch_sizes[resource], offset=count)

      message("Getting ", resource, " from the We The People API. URL: ", fully_qualified_url, " PARAMS: ", toJSON(params))

      resources_raw <- fromJSON(getForm(fully_qualified_url, .params=params))

      result_count <- length(resources_raw$results)
      message("Loaded ", result_count, " resources")

      if(result_count == 0) {
        break
      }

      if(resource=='petitions') {
        petitions <- ldply(
          resources_raw$results,
          function(item) {
            as.data.frame(unlist(item, recursive=FALSE), stringsAsFactors=FALSE)
          }
        )

        resource_df <- petitions
      }
      else if(resource == 'users') {
        resource_df <- users_from_json(resources_raw$results)
      }
      else {
        resource_df <- ldply(
          resources_raw$results,
          function(item) {
            as.data.frame(rbind(unlist(item, recursive=FALSE)), stringsAsFactors=FALSE)
          }
        )
      }

      if(is.null(result)) {
        result <- resource_df
      }
      else {
        result <- merge(result, resource_df, all=TRUE)
      }

      if(count == nrow(result)) {
        break
      }

      count <- nrow(result)

      message("New count: ", count)

      if(!is.na(limit) && count >= limit) {
        break
      }

    }

    if(!is.na(limit)) {
      result <- head(result, n=limit)
    }

    result <- add_datetime_fields(result)
    result

  }

  #' Retrieves signatures for the given petition_id.
  signatures <- function(petition_id, limit=NA) {
    get_resource('signatures', parent_id=petition_id, limit=limit)
  }

  #' Loads petitions from the API or from a flat JSON file.
  #' @param file optional file to load from instead of hitting the API
  #' @param limit to limit the number of petitions returned
  petitions <- function(file=NA, limit=NA) {
    get_resource('petitions', limit=limit)
  }

  users <- function(limit=NA) {
    get_resource('users', limit=limit)
  }

  interface <- list(
    petitions=petitions,
    signatures=signatures,
    users=users
  )

  class(interface) <- 'WeThePeople'

  interface

}

#' Transforms Users from JSON to a data.frame
#' @param users nested lists from the json representation
#' @return users data.frame
#' @export
#' @examples
#' data(users.from.json)
#' users <- from_json(users.from.json)
#' stopifnot(names(users) == c('type', 'id', 'created'))
users_from_json <- function(users) {
  results <- ldply(
    users,
    function(d) {
      as.data.frame(rbind(unlist(d)), stringsAsFactors=FALSE)
    }
  )
  results
}

add_datetime_fields <- function(entities) {

  for(field in c('created', 'deadline')) {
    if(field %in% names(entities)) {
      entities[[sprintf('%s_POSIXct', field)]] <- as.POSIXct(as.numeric(entities[[field]]), origin="1970-01-01")
    }
  }

  entities

}
