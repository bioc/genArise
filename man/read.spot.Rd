\name{read.spot}
\alias{read.spot}
\title{Read Spot from File}
\description{
Read all file, but only extract the interested columns and create a Spot object.
}
\usage{
read.spot(file.name, cy3 = 1, cy5 = 2, bg.cy3 = 3, bg.cy5 =
4, ids = 5, header = FALSE, sep = "\t", is.ifc = FALSE)
}
\arguments{
\item{file.name}{a connection or a character string giving the
name of the file to read where each column represent the spot components.}
\item{cy3}{column that represent Cy3.}
\item{cy5}{column that represent Cy5.}
\item{bg.cy3}{column that represent BgCy3.}
\item{bg.cy5}{column that represent BgCy5.}
\item{ids}{column that represent Id.}
\item{header}{the logical value of the header input file}
\item{sep}{the separator in the inputfile}
\item{is.ifc}{If is.ifc = TRUE this experiment was done in the
  Unit of Microarray  from  Cellular Phisiology Institute.}
}
\seealso{
\code{\link{write.spot}}.
}
\keyword{file}
