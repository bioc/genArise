\name{write.dataSet}
\alias{write.dataSet}
\title{Write dataSet}
\description{
  Write the values for observations of an object of DataSet class in an
  output file. This values are writen in columns with the follow order:
  Cy3, Cy5, Cy3 Background, Cy5 Background, Ids and finally the Zscore
  value. By default this output file has no header.}
\usage{
write.dataSet(dataSet.spot, fileName, quote
= FALSE, col.names = FALSE, row.names = FALSE,
Zscore.min = NULL, Zscore.max = NULL, sep = "\t")
}
\arguments{
\item{dataSet.spot}{An object of DataSet class}
\item{fileName}{The name of the output file where the data will be
  writen. This argument must be quoted.}
\item{quote}{If quote = TRUE, all values in the file will be quoted.}
\item{col.names}{If col.names = TRUE, an integer is writen in every
  column as header. By default col.names = FALSE.}
\item{row.names}{If row.names = TRUE will be an extra column that
  numerates every rows in the file.}
\item{Zscore.min}{The lower value in a range, if Zscore.min = NULL then
  the file will contain all values bellow Zscore.max}
\item{Zscore.max}{The greater value in a range, if Zscore.max = NULL
  then file will be contain all values above Zscore.min. Both values,
  Zscore.min and Zscore.max can not be NULL}
\item{sep}{Character to separate the columns in file. By default sep
  = "\t".}
}
\examples{
data(WT.dataset)
write.dataSet(dataSet.spot = WT.dataset, fileName = "Example.csv", Zscore.min = 1,
Zscore.max = 1.5, sep = "\t")
}
\keyword{file}
