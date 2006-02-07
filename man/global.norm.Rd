\name{global.norm}
\alias{global.norm}
\title{Global Normalization of Spot}
\description{
  This function normalize R and I values and fit the value of Cy5
  from his argument. In this function the normalize algorithm will be
  applied to all observations to get the lowess factor and then fit
  Cy5 with this factor. The observations. The observations with values
  R = 0 are deleted because they have no change in their expression levels.}
\usage{
global.norm(mySpot)
}
\arguments{
\item{mySpot}{A spot object}
}
\value{
A new spot object but normalized, It means with a different Cy5 that is
the result of the fit with the lowess factor.}
\examples{
data(Simon)
# Background Correction
c.spot <- bg.correct(Simon)
#Normalized data
n.spot <- global.norm(c.spot)
}
\keyword{arith}
\keyword{math}
