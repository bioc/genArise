\name{Zscore.plot}
\alias{Zscore.plot}	
\title{Z-score Data Visualization: R vs I or M vs A}
\description{
This function allows to plot \bold{R-values} vs \bold{I-values} or
\bold{M-values} vs \bold{A-values} for identifying differential expression.}
\usage{
Zscore.plot(dataSet.spot, Zscore.min, Zscore.max, all, col)
}
\arguments{
  \item{dataSet.spot}{Spot Object}
  \item{Zscore.min}{The lower value in a range, if Zscore.min = NULL then
    the file will contain all values bellow Zscore.max}
  \item{Zscore.max}{The greater value in a range, if Zscore.max = NULL
    then file will be contain all values above Zscore.min. Both values,
    Zscore.min and Zscore.max can not be NULL}
  \item{all} {Plot all the observations in four sets: Z < 1, 1 < Z <
    1.5, 1.5 < Z < 2, Z > 2}
    \item{col}{ Color in which the pioints of the plot will be shown where
    only the points from center are plot. This argument must be quoted
    and the possible values it can take are the same from the colors
    function in the R base package.}
  }
\examples{
data(WT.dataset)
Zscore.plot(WT.dataset, Zscore.min = 1, Zscore.max = 2)
}
\seealso{colors()}
\keyword{aplot}
