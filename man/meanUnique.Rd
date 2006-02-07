\name{meanUnique}
\alias{meanUnique}
\title{Remove Duplicates}
\description{
  This function allows to remove from the spot repeated Id's. Before
  moving one of the repeated Id's the function compute the average of
  Cy3 intensity and Cy5 intensity.
  
}
\usage{
meanUnique(mySpot)
}
\arguments{
\item{mySpot}{Spot object for one microarray.}
}
\value{
Spot object without duplicates
}
\examples{
data(Simon)
c.spot <- bg.correct(Simon)
n.spot <- global.norm(c.spot)
f.spot <- filter.spot(n.spot)
meanUnique(f.spot)
}
\keyword{math}

