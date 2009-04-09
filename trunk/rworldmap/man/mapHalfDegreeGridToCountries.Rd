\name{mapHalfDegreeGridToCountries}
\alias{mapHalfDegreeGridToCountries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Maps user half degree gridded data at country level by first aggregating.}
\description{
Maps user half degree gridded data at country level by first aggregating.
}
\usage{
mapHalfDegreeGridToCountries(
inFile=""
, aggregateOption="sum"  #"mean","max","min"
, nameCountryColumn = ""
, suggestForFailedCodes = FALSE 
, projection="EqualArea"  #"none", "EqualArea" 
, mapResolution="low" #                                                 
, numCats = 7 # *may be ignored by catMethod
, we=-160
, ea=160
, so=-80
, no=90 
, mapRegion = "world"   #sets map extents, overrides we,ea etc.
, catMethod="pretty"   #any vector defining breaks, "fixedWidth","quantiles"
, colourPalette= "heat" #"heat","white2Black","palette" for current palette
, addLegend=TRUE 
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inFile}{ either a gridascii filename or an sp SpatialGridDataFrame object specifying a global half degree grid dataset, if none specified an internal example data is used  }
  \item{aggregateOption}{ how to aggregate the data ('sum','mean','min','max') }
  \item{nameCountryColumn}{ optional name of column containing country names (used in reporting of success/failure) }
  \item{suggestForFailedCodes}{ T/F whether you want system to suggest for failed codes NOT YET WORKING }
  \item{projection}{ 'none' or 'EqualArea' }
  \item{mapResolution}{options low, medium, only for projection='none' initially}
  \item{numCats}{ number of categories, may be overided e.g. if catMethod ='pretty' }
  \item{we}{ map extents, can be overidden by mapRegion }
  \item{ea}{ map extents, can be overidden by mapRegion }
  \item{so}{ map extents, can be overidden by mapRegion }
  \item{no}{ map extents, can be overidden by mapRegion }
  \item{mapRegion}{ 'world','africa','oceania','eurasia','uk' sets map extents, overrides we,ea etc. }
  \item{catMethod}{ method for categorisation of data "pretty", any vector defining breaks, "fixedWidth","quantiles"  }
  \item{colourPalette}{ "heat","white2Black","palette":for current palette }
  \item{addLegend}{ whether to add a legend or not T/F }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
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
    
mapHalfDegreeGridToCountries( )              

#different agggregate option
mapHalfDegreeGridToCountries( aggregateOption="mean" )


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }

