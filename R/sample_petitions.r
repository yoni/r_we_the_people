#' Loads a small sample of petitions for use in examples.
#' @return a sample of 100 petitions
#' @export
#' @examples
#' petitions <- sample_petitions()
sample_petitions <- function() {
  WeThePeopleClient()$petitions(file=system.file('extdata/petitions.json', package='wethepeople'))
}
