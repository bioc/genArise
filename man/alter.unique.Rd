\name{alter.unique}
\alias{alter.unique}
\title{Remove Duplicates}
\description{
This function allows to remove from the spot repeated Id's. Before
moving one of the repeated Id's the function compute the log ratio of
both values with the same Id and delete the least absolute value if both
of them are positive or negative. In other case delete both observations. 
}
\syntax{
  alter.unique(mySpot)
}
\usage{
alter.unique(mySpot)
}
\arguments{
\item{mySpot}{Spot object for one microarray.}
}
\value{
Spot object without duplicates.
}
\examples{
data(Simon)
## filter the spot and save it in f.spot
f.spot <- filter.spot(Simon)
## remove duplicates and save it in u.spot
u.spot <- alter.unique(f.spot)
}
\keyword{math}
\keyword{arith}
