\name{Zscore}
\alias{Zscore}
\title{Z-scores for identifying differential expression}
\description{
  This function identify differential expressed genes by calculating an
  intensity-dependent Z-score. This function use a sliding window to
  calculate the mean and standard deviation within a window surrounding
  each data point, and define a Z-score where Z measures the number of
  standard deviations a data point is from the mean.}
\syntax{
Zscore(spot.object)
}
\usage{
Zscore(spot.object,type,window.size)
}
\arguments{
  \item{spot.object}{A spot object}
  \item{type}{Type of analysis: "ri" is for a R-I analysis and "ma" is
    for M-A analysis}
  \item{window.size}{Size of the sliding window}
}
\value{
A dataSet object with attributes Cy3, Cy5, Id, Z-score.}
\examples{
data(Simon)
# Background Correction
c.spot <- bg.correct(Simon)
#Normalized data
n.spot <- grid.norm(c.spot,23,24)
#Filter spot
f.spot <- filter.spot(n.spot)
#Replicate filtering
u.spot <- spotUnique(f.spot)
#Zscore analysis
s.spot <- Zscore(u.spot)
}
\keyword{arith}
\keyword{math}
