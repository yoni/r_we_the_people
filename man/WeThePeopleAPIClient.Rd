\name{WeThePeopleAPIClient}
\alias{WeThePeopleAPIClient}
\title{We The People API Client, based on RCurl.
see RCurl}
\usage{
  WeThePeopleAPIClient()
}
\value{
  WeThePeople Object with methods for interfacing with the
  API
}
\description{
  We The People API Client, based on RCurl. see RCurl
}
\examples{
client <- WeThePeopleAPIClient()
petitions <- client$petitions(limit=1)
signatures <- client$signatures(petitions)
}

