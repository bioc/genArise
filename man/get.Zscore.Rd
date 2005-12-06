\name{get.Zscore}
\alias{get.Zscore}
\title{Swap from Files}
\description{
Read both files, but only extract the interested columns and create a Spot object.
}
\usage{
get.Zscore( spot, name, Zscore.min=NULL, Zscore.max=NULL, all=FALSE, envir)
}
\arguments{
\item{spot}{a connection or a character string giving the
  name of the file to read where each column represent the spot
  components.}
\item{name}{a connection or a character string giving the
  name of the file to read where each column represent the spot
  components.}
\item{Zscore.min}{column that represent Cy3.}
\item{Zscore.max}{column that represent Cy5.}
\item{all}{column that represent BgCy3.}
\item{envir}{Environment where are the genArise variables.}
}
\seealso{
\code{\link{write.spot}}.
}
\keyword{file}
