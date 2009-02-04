\name{write.zscore}
\alias{write.zscore}
\title{Write Z-score data}
\description{
  Write the values for observations of an object of DataSet class in an
  output file. This values are writen in columns tab separated with the
  follow order:
  Cy3, Cy5, Cy3 Background, Cy5 Background, Ids and finally the z-score
  value. The header of the output file is the selected type for the
  z-score (ri or ma).}
\usage{
write.zscore(dataSet.spot, fileName, sep = "\t")
}
\arguments{
\item{dataSet.spot}{An object of DataSet class}
\item{fileName}{The name of the output file where the data will be
  writen. This argument must be quoted.}
\item{sep}{Character to separate the columns in file. By default sep = "\\t".}
}
\examples{
data(WT.dataset)
write.zscore(dataSet.spot = WT.dataset, fileName = "Zscore.csv", sep =
"\t")
}
\keyword{file}
