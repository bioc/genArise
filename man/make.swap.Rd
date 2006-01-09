\name{make.swap}
\alias{make.swap}
\title{Swap analysis}
\description{
Read both files, but only extract the interested columns and create a Spot object.
}
\usage{
make.swap(spot1, spot2, Cy3, Cy5, BgCy3, BgCy5, Id, Symdesc, header = FALSE, is.ifc = FALSE,envir,nr,nc)
}
\arguments{
\item{spot1}{a connection or a character string giving the
  name of the file to read where each column represent the spot
  components.}
\item{spot2}{a connection or a character string giving the
  name of the file to read where each column represent the spot
  components.}
\item{Cy3}{column that represent Cy3.}
\item{Cy5}{column that represent Cy5.}
\item{BgCy3}{column that represent BgCy3.}
\item{BgCy5}{column that represent BgCy5.}
\item{Id}{column that represent Id.}
\item{Symdesc}{optional identifier besides the Id column.}
\item{header}{the logical value of the header input file}
\item{is.ifc}{If is.ifc = TRUE this experiment was done in the
  Unit of Microarray  from  Cellular Phisiology Institute.}
\item{envir}{Environment where are the genArise variables.}
\item{nr}{Total of meta-row.}
\item{nc}{Total of meta-column.}
}
\seealso{
\code{\link{write.spot}}.
}
\keyword{file}
