\name{mapGridAscii}
\alias{mapGridAscii}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ To map user gridded data }
\description{
To map user gridded data, either a gridascii filename or an sp SpatialGridDataFrame object specifying a global half degree grid dataset
}
\usage{
mapGridAscii(inFile = "", numCats = 10, logIndex = T, we = -180, ea = 180, so = -90, no = 90, numHistBarsPerCat = 1, addTo0ForLog = 0.01, mapRegion = "world", titleHist = "", addHist = F, addLegend = T, addCountryOutlines = T, plotData = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inFile}{ either a gridascii filename or an sp SpatialGridDataFrame object specifying a global half degree grid dataset, if none specified an internal example data is used  }
  \item{numCats}{ number of categories to put the data in, may be overidden if catMethod ='pretty' } 
  \item{we}{ map extents, can be overidden by mapRegion }
  \item{ea}{ map extents, can be overidden by mapRegion }
  \item{so}{ map extents, can be overidden by mapRegion }
  \item{no}{ map extents, can be overidden by mapRegion }
  \item{numHistBarsPerCat}{ if adding a histogram this sets number of histogram bars per category }
  \item{addTo0ForLog}{ if logIndex is set to true, this is the value added to zero's before log calculated }
  \item{mapRegion}{ 'world','africa','oceania','eurasia','uk' sets map extents, overrides we,ea etc. }
  \item{titleHist}{ if adding a histogram this sets title for histogram }
  \item{addHist}{ T/F whether to add a histogram under the map }
  \item{addLegend}{ T/F whether to add a legend to the map }
  \item{addCountryOutlines}{ T/F whether to add country outlines to the map }
  \item{plotData}{ T/F whether to plot the data on the map }
  \item{catMethod}{ method for categorisation of data "pretty", "fixedWidth", "logfixedWidth","quantiles", or a numeric vector defining breaks  } 
  \item{colourPalette}{ "heat","white2Black","palette":for current palette }
  
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
\references{ }
\author{ andy south }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

mapGridAscii()

mapGridAscii(addHist=T)    
    
mapGridAscii(mapRegion="africa")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
