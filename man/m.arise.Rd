\name{m.arise}
\alias{m.arise}
\title{M Arise}
\description{
Extract \bold{M values} from a Spot.
}
\usage{
m.arise(mySpot)
}
\arguments{
\item{mySpot}{Spot object for one microarray.}
}
\value{
List of M-values
}
\seealso{
\code{\link{a.arise}}.
}
\examples{
data(Simon)
## Extract M from spot and save in m
m <- m.arise(Simon)
}
\keyword{math}
\keyword{arith}
