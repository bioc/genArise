\name{Simon}
\alias{Simon}
\title{Dataset: Little fragment of a microarray from IFC UNAM}
\description{
  This structure is a data fragment of a yeast microarray from the
  Microarrays Unit in IFC UNAM. The original microarray contains 6
  meta-rows and 4 meta-columns, however this data just belongs to the
  first meta-row order in a way of 2 meta-rows and 2 meta-columns.} 
\usage{data(Simon)}
\format{A list that contains 1104 observations, because the dimensions
  of this example are: 2 meta-rows, 2 meta-columns, 23 rows, 24 columns.}
\examples{
data(Simon)
#A preview from the chip
datos <- attr(Simon, "spotData")
M <- log(datos$Cy3, 2) - log(datos$Cy5, 2)
imageLimma(M, 23, 24, 2, 2)
}
\keyword{datasets}
