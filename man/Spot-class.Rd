\name{Spot-class}
\docType{class}
\alias{Spot-class}
\title{Spot - class}

\description{
A simple list-based class for storing red and green channel foreground
and background intensities for a batch of spotted microarrays and the Ids.
}

\section{Creating Objects from the Class}{
Objects can be created by calls of the form \code{new("Spot",spot)} where \code{spot} is a list.
Objects are normally created by \code{\link{read.spot}}.
}

\section{Slots/List Components}{
This class contains no slots (other than \code{.Data}), but objects should contain the following list components:
\tabular{ll}{
  \code{Cy5}:\tab numeric matrix containing the red (cy5) foreground intensities.  Rows correspond to spots and columns to arrays.\cr
  \code{Cy3}:\tab numeric matrix containing the green (cy3) foreground intensities.\cr
  \code{BgCy5}:\tab numeric matrix containing the red (cy5) background intensities.\cr
  \code{BgCy3}:\tab numeric matrix containing the green background intensities.\cr
  \code{Id}:\tab Ids from all the observations.
}
All of these matrices should have the same dimensions.
}

\section{Methods}{
This class inherits directly from class \code{list} so any operation appropriate for lists will work on objects of this class.
}

\keyword{classes}
\keyword{data}
