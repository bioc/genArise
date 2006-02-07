\name{cys.plot}
\alias{cys.plot}
\title{Data Visualization: log2(Cy3) vs log2(Cy5)}
\description{
  This function shows the plot of the values from the log Cy3 against
  log Cy5 intensities that belongs to an object of the Spot class.}
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
