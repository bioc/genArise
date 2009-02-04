\name{write.spot}
\alias{write.spot}
\title{Write Spot}
\description{
  Write the values for observations of an object of Spot class in an
  output file. This values are writen in columns with the follow order:
  Cy3, Cy5, Cy3 Background, Cy5 Background and finally Ids. By default
  this file has no header.}
\usage{
write.spot(spot, fileName, quote = FALSE,sep = "\t",
col.names = FALSE, row.names = FALSE)
}
\arguments{
\item{spot}{An object of Spot class}
\item{fileName}{The name of the output file where the data will be
  writen. This argument must be quoted.}
\item{quote}{If quote = TRUE, all values in the file will be quoted.}
\item{sep}{Character to separate the columns in file. By default sep = "\\t".}
\item{col.names}{If col.names = TRUE, an integer is writen in every
  column as header. By default col.names = FALSE.}
\item{row.names}{If row.names = TRUE will be an extra column that
  numerates every rows in the file.}
\code{\link{read.spot}}.
}
\examples{
data(Simon)
write.spot(spot = Simon, fileName = "Example.csv", quote = FALSE, sep =
"\t", col.names = FALSE, row.names = FALSE)
}
\keyword{file}
