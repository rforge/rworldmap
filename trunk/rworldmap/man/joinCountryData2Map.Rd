\name{joinCountryData2Map}
\alias{joinCountryData2Map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Joins user country referenced data to a map }
\description{
Joins user data referenced by country codes to internal map ready for plotting, reports on which countries joined.
}
\usage{
joinCountryData2Map(dF
, joinCode = "ISO3" 
, nameJoinColumn = "ISO3V10"
, nameCountryColumn = "Country"
, suggestForFailedCodes = FALSE 
, projection="EqualArea"  #options "none", "EqualArea"
, mapResolution="low" 
, verbose = TRUE   
)
 }

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dF}{ R data frame with at least one column for country reference and one column of data }
  \item{joinCode}{ how countries are referenced options "ISO2","ISO3","FIPS","NAME", "UN" = numeric codes }
  \item{nameJoinColumn}{ name of column containing country referencing }
  \item{nameCountryColumn}{ optional name of column containing country names (used in reporting of success/failure) }
  \item{suggestForFailedCodes}{ T/F whether you want system to suggest for failed codes NOT YET WORKING }
  \item{projection}{ which map projection to use, options 'none', 'EqualArea' latter uses Mollweide  }
  \item{mapResolution}{options low, medium, only for projection='none' initially}
  \item{verbose}{if set to FALSE it doesn't print progress messages to console}
  }
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  An R 'SpatialPolygonsDataFrame' [package "sp"] object with the data joined to it 
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


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }

