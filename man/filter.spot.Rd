\name{filter.spot}
\alias{filter.spot}
\title{Intensity-based filtering of array elements}
\description{
  This function keep only array elements with intensities that are 2
  standard deviation above background.
}
\usage{
filter.spot(mySpot)
}
\arguments{
  \item{mySpot}{Spot object for one microarray.}
}
\value{
  Array elements with intensities that are 2 standard deviation above
  background.
}
\references{
  John Quackenbush "Microarray data normalization and transformation".
  Nature Genetics. Vol.32 supplement pp496-501 (2002)
  }
\examples{
data(Simon)
## background correction and save it in c.spot
c.spot <- bg.correct(Simon)
## normalize spot
n.spot <- grid.norm(c.spot, nr = 23, nc = 24)
## filtering the spot
filter.spot(n.spot)
}
\keyword{arith}
\keyword{math}
