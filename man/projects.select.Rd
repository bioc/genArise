\name{project.select}
\alias{projects.select}
\title{File selector}
\description{
  Previous window to post-analysis. In this window you can select one or
  several files (projects) and arguments to be used by post analisis
  function.
  
  This is just an auxiliar function for genAriseGUI, and can not be used
  in the command line.
}
\usage{
projects.select(envir, nombre)
}
\arguments{
  \item{envir}{Environment where are the genArise variables}
  \item{nombre}{Name of directory where the post-analysis results will
    be placed.}
}
\value{
  tkwidget
}
\keyword{file}