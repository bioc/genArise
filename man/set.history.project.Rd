\name{set.history.project}
\alias{set.history.project}
\title{Save the history of a project}
\description{
Save in the history file each operation performed while the
analysis. This is just to get the open this particular project in the
future. This is just an auxiliary function for the GUI, and can not be
used in the command line.
}
\usage{
set.history.project(history.file, id.name, data file)
}
\arguments{
  \item{history.file}{The name of the prj history file.}
  \item{id.name}{The name of the operation.}
  \item{data.file}{The file with the results of the operation.}
}
\value{
  The history file with the new performed operation.
}
\keyword{file}