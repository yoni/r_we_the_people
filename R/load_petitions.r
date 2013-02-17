#' Loads petition data from a JSON file.
#' @param file the path to the JSON file, assumed to be in the format of a We The People API result
#' @return petitions a data frame of the petitions
#' @importFrom rjson fromJSON
#' @export
load_petitions <- function(file) {

  petitions_raw <- fromJSON(file=file)

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
