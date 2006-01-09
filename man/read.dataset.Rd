\name{read.dataset}
\alias{read.dataset}
\title{Read Dataset from File}
\description{
Read all file and extract the interested columns to create a DataSet
object (this file contain the zscore with all the genes after the
duplicates filtering and makes not distinction between up-regulated
and down-regulated. If you want to make this distinction you must write
the data with the function write.dataSet, but there is no way to read
this files with this function).
}
\usage{
read.dataset(file.name, cy3 = 1, cy5 = 2, ids = 3, symdesc = 4, zscore =
5, type = 6, header = FALSE, sep = "\t")
}
\arguments{
\item{file.name}{a connection or a character string giving the
name of the file to read where each column represent the dataset components.}
\item{cy3}{column that represent Cy3.}
\item{cy5}{column that represent Cy5.}
\item{ids}{column that represent Id.}
\item{symdesc}{optional identifier besides Id column.}
\item{zscore}{column that represent the zscore value.}
\item{type}{column that represent if the experiment was performed as R
  vs I or M vs A.}
\item{header}{the logical value of the header input file}
\item{sep}{the separator in the inputfile}
}
\seealso{
\code{\link{write.zscore}}.
}
\keyword{file}

