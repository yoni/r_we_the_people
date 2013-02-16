#' Loads petition data from a JSON file.
#' @param file the path to the JSON file, assumed to be in the format of a We The People API result
#' @return petitions a data frame of the petitions
#' @export
load_petitions <- function(file) {
  petitions = fromJSON(file=file)
  items <- ldply(petitions$results, function(item) { as.data.frame(unlist(item, recursive=FALSE)) })
  for(field in c('created', 'deadline')) {
    items[[sprintf('%s_POSIXct', field)]] <- as.POSIXct(items[[field]], origin="1970-01-01")
  }
  items
}
