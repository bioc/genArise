\name{spotUnique}
\alias{spotUnique}
\title{Replicate filtering}
\description{
We consider replicate measures of two samples and adjust the
log(ratio,2) measures for each gene so that the transformed values are
equal. To do this we take the geometric mean.\\
This procedure can be extended to averaging over \code{n} replicates.
}
\syntax{
spotUnique(mySpot)
}
\usage{
spotUnique(mySpot)
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
f.spot <- filter.spot(c.spot)
spotUnique(mySpot = f.spot)
}
\keyword{math}

