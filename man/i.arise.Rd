\name{i.arise}
\alias{i.arise}
\title{I Arise}
\description{
Extract \bold{I} from a Spot.
}
\usage{
i.arise(mySpot)
}
\arguments{
\item{mySpot}{Spot object for one microarray.}
}
\value{
List of I-values
}
\seealso{
\code{\link{r.arise}}.
}
\examples{
data(Simon)
## Extract I from spot and save in i
i.arise(Simon)
}
\keyword{math}
