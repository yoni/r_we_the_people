\name{WeThePeopleClient}
\alias{WeThePeopleClient}
\title{We The People API Client, based on RCurl.
see RCurl}
\usage{
  WeThePeopleClient()
}
\value{
  WeThePeople Object with methods for interfacing with the
  API
}
\description{
  We The People API Client, based on RCurl. see RCurl
}
\examples{
client <- WeThePeopleClient()
petitions <- client$petitions(limit=1)
signatures <- client$signatures(petitions)
}

