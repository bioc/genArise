\name{ri.plot}
\alias{ri.plot}	
\title{Data Visualization: R vs I}
\description{
This function allows to plot \bold{R-values} vs \bold{I-values}
\bold{I-value} from a Spot object}
\syntaxis{
ri.plot(mySpot, col = "green")
}
\usage{
ri.plot(mySpot, col = "green")
}
\arguments{
  \item{mySpot}{Spot Object}
  \item{col}{ Color in which the pioints of the plot will be shown. This
    argment must be quoted and the possible values it can ake ares the
    same from the colors funcion in the R base package.}
  }
\examples{
data(Simon)
ri.plot(Simon)
}
\seealso{colors()}
\keyword{aplot}
