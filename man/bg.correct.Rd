\name{bg.correct}
\alias{bg.correct}
\title{Background Correction}
\description{
This function use the background data to eliminate unwanted effects in signal.
The background correction establish the new Cy3 signal as Cy3 - BgCy3 and 
the new Cy5 as Cy5 - BgCy5.
}
\usage{
bg.correct(mySpot)
}
\arguments{
\item{mySpot}{Spot object for one microarray.}
}
\value{
Spot object with the background correction done. 
}
\examples{
data(Simon)
## background correction and save it in c.spot
c.spot <- bg.correct(Simon)
}
\keyword{math}

