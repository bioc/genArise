\name{r.arise}
\alias{r.arise}
\title{Get R value}
\description{
Get the \bold{R values} from an object of the Spot class.}
\usage{
r.arise(mySpot)
}
\arguments{
\item{mySpot}{An object of the Spot class}
}
\value{
A vector containing the R value ( log(Cy5/Cy3)) for each observation of
the spot object.}
\seealso{
\code{\link{i.arise}}.
}
\examples{
data(Simon)
#Get R-value from an object of the Spot class and save the result
R <- r.arise(Simon)
#Show the R-values}
\keyword{math}
\keyword{arith}
