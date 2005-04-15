\name{imageLimma}
\alias{imageLimma}
\title{Image Plot of Microarray}
\description{
  Plot an image of colours representing the log intensity ratio for each 
  spot on the array. This function can be used to explore whether there
  are any spatial effects in the data.
}
\usage{
imageLimma(z, row, column, meta.row, meta.column,
low = NULL, high = NULL)
}
\arguments{
  \item{z}{numeric vector or array. This vector can contain any spot
    statistics, such as log intensity ratios, spot sizes or
    shapes, or t-statistics. Missing values  are allowed and will
    result in blank spots on the image }
  \item{row}{rows in the microarray }
  \item{column}{columns in the microarray }
  \item{meta.row}{ metarows in the microarray}
  \item{meta.column}{ metacolumns in the microarray}
  \item{low}{color associated with low values of 'z'. May be specified as
    a character string  such as '"green"', '"white"' etc, or as a
    rgb vector in which 'c(1,0,0)' is red,  'c(0,1,0)' is green
    and 'c(0,0,1)' is blue. The default value is '"green"' if
    'zerocenter=T' or '"white"' if 'zerocenter=F'.}
  \item{high}{color associated with high values of 'z'. The default value
    is '"red"' if 'zerocenter=T' or '"blue"' if 'zerocenter=F'.}
}
\note{This function is based in the imageplot function from limma package.}
\references{
  Gordon K. Smyth (2004) "Linear Models and Empirical Bayes Methods for
  Assessing Differential Expression in Microarray Experiments",
  Statistical Applications in Genetics and Molecular Biology: Vol. 3:
  No. 1, Article 3.
  \url{http://www.bepress.com/sagmb/vol3/iss1/art3}
}
\examples{
data(Simon)
spot.data <- attr(Simon, "spotData")
M <- log(spot.data$Cy5, 2) - log(spot.data$Cy3, 2)
imageLimma(z = M, row = 23, column = 24, meta.row = 2, meta.column = 2,
low = NULL, high = NULL)
}
\keyword{color}
