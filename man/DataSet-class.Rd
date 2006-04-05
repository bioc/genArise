\name{DataSet-class}
\docType{class}
\alias{DataSet-class}
\title{DataSet - class}

\description{
A simple list-based class for storing red and green channel foreground,
z-scores and the Ids.
}

\section{Creating Objects from the Class}{
Objects can be created by calls of the form \code{new("DataSet",sets, type)}
where \code{sets} is a list containing Cy3, Cy5, Id and Zscore and
\code{type} is "ri" or "ma".
Objects are normally created by \code{\link{read.spot}}.
}

\section{Slots/List Components}{
This class contains no slots (other than \code{.Data}), but objects should contain the following list components:
\tabular{ll}{
  \code{Cy5}:\tab numeric matrix containing the red (cy5) foreground
  intensities.  Rows correspond
  to spots and columns to arrays.\cr
  \code{Cy3}:\tab numeric matrix containing the green (cy3) foreground intensities.\cr
  \code{Id}:\tab Ids from all the observations.\cr
  \code{Zscore}:\tab The result of (R - mean) / sd that define an
  intensity-dependent Z-score threshold to identify differential expression.
}
All of these matrices should have the same dimensions.
}

\section{Methods}{
This class inherits directly from class \code{list} so any operation appropriate for lists will work on objects of this class.
}

\keyword{classes}
\keyword{data}
