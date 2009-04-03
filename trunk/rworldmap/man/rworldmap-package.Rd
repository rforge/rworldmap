\name{rworldmap-package}
\alias{rworldmap-package}
\alias{rworldmap}
\docType{package}
\title{
For mapping global data.
}
\description{
Enables mapping of country level and gridded user datasets by facilitating joining to world maps and visualisation options.  
}
\details{
\tabular{ll}{
Package: \tab rworldmap\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-08-05\cr
License: \tab What license is it under?\cr
}
~~ An overview of how to use the package ~~
}
\author{
andy south

Maintainer: Who to complain to <andy.south@cefas.co.uk>
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{

#mapping country level data, with no file specified it uses internal example data
mapHalfDegreeGridToCountries() 
#specifying region
mapCountryData(mapRegion="asia",projection="none")
#mapping gridded data, with no file specified it uses internal example data
mapGridAscii()
#adding histogram
mapGridAscii(addHist=T)  
#specifying region 
mapGridAscii(mapRegion="africa")  
#aggregating gridded data to country level, with no file specified it uses internal example data
mapHalfDegreeGridToCountries()              
#different agggregate option
mapHalfDegreeGridToCountries( aggregateOption="mean" )

}
