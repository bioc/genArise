\name{get.values}
\alias{get.values}
\title{Auxiliar function for post-analysis}
\description{
  This function get values from an DataSet object.
  
  This is just a function for the GUI, and can not be
  used in the command line.
}
\usage{
get.values(list.values, genes.values, up.down, min.val, max.val)
}
\arguments{
  \item{list.values}{Zscore values from DataSet object}
  \item{genes.values}{Ids values from DataSet object}
  \item{up.down}{If the analysis will be done with "up" or "down" regulated}
  \item{min.val}{Minimal value of the range}
  \item{max.val}{Maximal value of the range}
}
\value{
  An Ids list
 }
\keyword{arith}
