\name{WeThePeopleClient}
\alias{WeThePeopleClient}
\title{We The People API Client, based on RCurl.
see RCurl}
\usage{
  WeThePeopleClient(key = "")
}
\arguments{
  \item{key}{optionsl We the People API Key. All functions
  except for API calls should work without the key.}
}
\value{
  WeThePeople Object with methods for interfacing with the
  API
}
\description{
  We The People API Client, based on RCurl. see RCurl
}
\examples{
client <- WeThePeopleClient('MY_API_KEY')
}

