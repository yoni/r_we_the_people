#' Loads a small sample of petitions for use in examples.
#' @return a sample of 100 petitions
#' @export
#' @examples
#' petitions <- load_sample_petitions()
load_sample_petitions <- function()
  load_petitions(file=system.file('data/petitions.json', package='wethepeople'))
