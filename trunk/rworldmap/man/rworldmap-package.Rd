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
Version: \tab 1.05\cr
Date: \tab 2009-12-15\cr
License: \tab GPL (>= 2)\cr
}
Country Level Data can be joined to a map using  \code{\link{joinCountryData2Map}}, then mapped using \code{\link{mapCountryData}}.
Gridded data can be mapped using \code{\link{mapGriddedData}}.

Type vignette('rworldmap') to access a short document showing a few examples of 
the main rworldmap functions to get you started.

}
\author{
Andy South

with contributions from Barry Rowlingson, Roger Bivand, Matthew Staines and Pru Foster 

Maintainer: <andy.south [at] cefas.co.uk>
}
\references{
http://code.google.com/p/rworld
}

\keyword{ package }


\examples{

#mapping country level data, with no file specified it uses internal example data
mapCountryData()
#specifying region
mapCountryData(mapRegion="asia")
#mapping gridded data, with no file specified it uses internal example data
mapGriddedData()
#specifying region 
mapGriddedData(mapRegion="africa")  
#aggregating gridded data to country level 
#with no file specified it uses internal example data
mapHalfDegreeGridToCountries()              


}
