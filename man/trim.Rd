\name{trim}
\alias{trim}
\title{Trim}
\description{
  Extract white spaces at the begining or end of a word.
} 
\usage{
trim(word)
}
\arguments{
  \item{word}{A string of characters posibly with white spaces at
    the beging or end of the string.  }
}
\value{
 Returns a string of characters, with leading and trailing whitespace omitted.
}
\examples{

trim("        This is a String            ")
## return [1] "This is a String"

}
\keyword{character}