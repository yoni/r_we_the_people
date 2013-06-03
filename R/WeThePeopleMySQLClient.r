#' We The People MySQL Client
#' @param user the MySQL username
#' @param password the MySQL password
#' @param host the MySQL host
#' @param dbname the MySQL database name
#' @return WeThePeopleMySQLClient Object with methods for interfacing with the MySQL database instance
#' @export
WeThePeopleMySQLClient <- function(user, password, host, dbname) {

  con <- dbConnect(MySQL(), user=user, password=password, dbname=dbname)

  getQuery <- function(query)
    dbGetQuery(query)

  getAll <- function(table) {
    getQuery(sprintf("select * from %s" , table))
  }

  petitions <- function() {
    getAll('wtp_data_petitions')
  }

  issues <- function() {
    getAll('wtp_data_issues')
  }

  petition_issues <- function() {
    getQuery("select * from wtp_data_petitions join wtp_data_petition_issues on wtp_data_petitions.id = wtp_data_petition_issues.petition_id join wtp_data_issues on wtp_data_issues.id = wtp_data_petition_issues.issue_id")
  }

  issues_by_status <- function() {
    getQuery("select wtp_data_issues.name, count(*), status from wtp_data_petitions join wtp_data_petition_issues on wtp_data_petitions.id = wtp_data_petition_issues.petition_id join wtp_data_issues on wtp_data_issues.id = wtp_data_petition_issues.issue_id group by wtp_data_issues.name, status")
  }

  interface <- list(
    petitions=petitions,
    issues=issues,
    petition_issues=petition_issues,
    issues_by_status=issues_by_status,
    con=con
  )

  class(interface) <- 'WeThePeopleMySQLClient'

  interface

}

