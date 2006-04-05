\name{annotations}
\alias{annotations}
\title{Gene Annotations}
\description{
  Performed an HTML file
}
\usage{
annotations(specie.data, specie, column, symbol,
output.file = "annotations.html")
}
\arguments{
  \item{specie.data}{A data frame}
  \item{specie}{Name of specie}
  \item{column}{Number of column where are the gene name in the data
    frame}
  \item{symbol}{An optional symbol besides GenBank ID}
  \item{output.file}{Name of output file }
}
\value{
  HTML file with link for each spot in data frame
}
\keyword{math}
