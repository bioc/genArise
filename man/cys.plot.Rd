\name{cys.plot}
\alias{cys.plot}
\title{Data Visualization: Cy3 vs Cy5}
\description{
  This function shows the plot of the values from the Cy3 against
  Cy5 intensities that belongs to an object of the Spot class.}
\syntaxis{
cys.plot(mySpot, col = "green")
}
\usage{
cys.plot(mySpot, col = "green")
}
\arguments{
  \item{mySpot}{An Spot object}
  \item{col}{Color in which the points of the plot will be shown. This
    argument must be quoted and the possible values it can take are the
    same from the color funcion in the R base.}
}
\examples{
data(Simon)
cys.plot(Simon)
}
\keyword{graphs}
\keyword{aplot}
