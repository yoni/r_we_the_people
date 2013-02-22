\name{from_json}
\alias{from_json}
\title{Transforms Users from JSON to a data.frame}
\usage{
  from_json(users)
}
\arguments{
  \item{users}{nested lists from the json representation}
}
\value{
  users data.frame
}
\description{
  Transforms Users from JSON to a data.frame
}
\examples{
data(users.from.json)
users <- from_json(users.from.json)
stopifnot(names(users) == c('type', 'id', 'created'))
}

