\name{post.analysis}
\alias{post.analysis}
\title{Set-combinatorial analysis}
\description{
  This function allows you to perform a set combinatorial analysis
  between the results previously obtained in different projects.
  This function is called post.analysis and it is mandatory that
  you have done the Zscore operation in all the selected projects. It is
  important to clarify that this function receives a list of files with
  extension \texttt{prj} as ergument and for this reason you can't use
  it if the results to compare was not obtained by the genArise GUI.
}
\usage{
post.analysis(values, min.val, max.val, up.down, output)
}
\arguments{
  \item{values}{A list of projects to compare}
  \item{min.val}{The minimal value of the range}
  \item{max.val}{The maximal value of the range}
  \item{up.down}{If the analysis will be done with "up" or "down" regulated}
  \item{output}{The directory that will contain all the output files}
  
  
}
\value{
  Once obtained the ids list for each project a number of files with
  extension set are created in a directory. The name of this files
  consists in a sequence of 0 and 1. The number of digits in the file
  names is the same to the number of projects in the list passed as
  argument to the function. There is then, a relation between the number
  of digits in the file names and the projects. This relation is defined
  by the position specified in the file order.txt in the same directory
  you have passed as another argument in the function.
}
\keyword{file}
