#' wethepeople is an R package for working with the White House's We The People petition data.
#'
#' @name wethepeople
#' @title A package for working with the We The People data.
#' @docType package
#' @author Yoni Ben-Meshulam \email{yoni@@opower.com}
#' @examples
#' # Load a sample set of petitions and generate some plots:
#' data(petitions)
#' plot_issues_over_time(petitions)
#' plot_wordcloud(petitions, 'title')
#'
#' # Create an API Client
#' client <- WeThePeopleClient('SOME_API_KEY')
NULL

#' Petitions loaded Feb 19, 2013
#'
#' Sample petitions loaded from the API on February 19, 2013.
#'
#' @name petitions
#' @docType data
#' @author Yoni Ben-Meshulam \email{yoni@@opower.com}
#' @keywords data
NULL

#' Signatures loaded Feb 19, 2013
#'
#' Sample signatures loaded from the API on February 19, 2013.
#'
#' @name signatures
#' @docType data
#' @author Yoni Ben-Meshulam \email{yoni@@opower.com}
#' @keywords data
NULL
