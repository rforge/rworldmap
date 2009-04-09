\name{aggregateHalfDegreeGridToCountries}
\alias{aggregateHalfDegreeGridToCountries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Aggregates global half degree gridded data to countries }
\description{
Aggregates global half degree gridded data to countries (options for sum, mean, min, max )
}
\usage{
aggregateHalfDegreeGridToCountries(inFile = "", aggregateOption = "sum")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inFile}{ either a gridascii filename or an sp SpatialGridDataFrame object specifying a global half degree grid dataset }
  \item{aggregateOption}{ how to aggregate the data ('sum','mean','min','max') }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  a dataframe with 2 columns : numeric country codes and the aggregated value for each country
}
\references{ ~put references to the literature/web site here ~ }
\author{ andy south }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
#source can go in here

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }

