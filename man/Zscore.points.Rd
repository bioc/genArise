\name{Zscore.points}
\alias{Zscore.points}	
\title{Z-score Window}
\description{
  This function display the window that show the results after the
  Z-score. This window allow:

  1. Show the plots of the up and down generated with the function Zscore.plot
  regulated spots in:
  Zscore < 1 sd
  1 sd < Zscore < 1.5 sd
  1.5 sd < Zscore < 2 sd
  Zscore > 2 sd
  and All the points

  2. Save the plots in pdf and save the results in an output file
  
  3. Gene annotations. Denote any gene information beyond the expression
  level data.

  This is just a function for the GUI, and can not be
  used in the command line.
}
\usage{
  Zscore.points(type,text,envir)
}
\arguments{
  \item{type}{Type of analysis done: "ri" is for a R-I analysis and "ma" is
    for M-A analysis}
  \item{text}{The text for the text area of the history of the project}
  \item{envir}{Environment where the variables are stored}
  }
\keyword{aplot}
