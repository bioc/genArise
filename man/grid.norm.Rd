\name{grid.norm}
\alias{grid.norm}
\title{Normalization by grid of Spot}
\description{
This function normalize R and I values and fit the value of Cy5 for each
grid in the spot that it receives as argument. In this function the
dimension of grid is (meta-row * meta-column).}
\usage{
grid.norm(mySpot, nr, nc)
}
\arguments{
\item{mySpot}{Spot object for one microarray.}
\item{nr}{Total of meta-row.}
\item{nc}{Total of meta-column.}
}
\value{
Spot object with the grid normalization done. 
}
\examples{
data(Simon)
## background correction and save it in c.spot
c.spot <- bg.correct(Simon)
## normalization and save it in n.spot
n.spot <- grid.norm(c.spot, 23, 24)
}
\keyword{arith}
\keyword{math}
