\name{a.arise}
\alias{a.arise}
\title{A Arise}
\description{
Extract \bold{A values} from a Spot.
}
\usage{
a.arise(mySpot)
}
\arguments{
\item{mySpot}{Spot object for one microarray.}
}
\value{
List of A-values. (log(cy3, 2) + log(cy5, 2))/2
}
\seealso{
\code{\link{m.arise}}.
}
\examples{
## read the spot from a file and save it in spot
data(Simon)
## Extract A from spot and save in a
a <- a.arise(mySpot = Simon)
}
\keyword{math}
\keyword{arith}
