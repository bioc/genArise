\name{principal}
\alias{principal}
\title{Principal window of genAriseGUI}
\description{
  This function show a window with the information of experiment like
  name and dimensions, too plot an  image of colours representing the
  log intensity ratio for each spot on the array.
  This is just an auxiliar function for genAriseGUI, and can not beused
  in the command line.
}
\usage{
principal(envir, swap)
}
\arguments{
  \item{envir}{Environment where are the genArise variables}
  \item{swap}{Is this a swap analysis or an individual analysis}
  }
\value{
  tkwidget
}
\keyword{color}