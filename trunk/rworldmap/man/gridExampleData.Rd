\name{gridExampleData}
\alias{gridExampleData}
\docType{data}
\title{ Example half degree grid data : population estimates for 2000 from IIASA (sp, SpatialGridDataFrame)}
\description{
Example half degree grid data : people per cell estimates for 2000 from IIASA (International Institute for Applied System Analysis) (sp, SpatialGridDataFrame). 
}
\usage{
data(gridExampleData)
#used as the default dataset in other functions, e.g. 
mapGridAscii()
}
\format{
  The format is:
Formal class 'SpatialGridDataFrame' [package "sp"] with 6 slots
  ..@ data       :'data.frame':	259200 obs. of  1 variable:
  .. ..$ pa2000.asc: num [1:259200] NA NA NA NA NA NA NA NA NA NA ...
  ..@ grid       :Formal class 'GridTopology' [package "sp"] with 3 slots
  .. .. ..@ cellcentre.offset: num [1:2] -179.8  -89.8
  .. .. ..@ cellsize         : num [1:2] 0.5 0.5
  .. .. ..@ cells.dim        : int [1:2] 720 360
  ..@ grid.index : int(0) 
  ..@ coords     : num [1:2, 1:2] -179.8  179.8  -89.8   89.8
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : chr [1:2] "coords.x1" "coords.x2"
  ..@ bbox       : num [1:2, 1:2] -180 -90 180 90
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:2] "coords.x1" "coords.x2"
  .. .. ..$ : chr [1:2] "min" "max"
  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots
  .. .. ..@ projargs: chr " +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
}
\details{
From International Institute for Applied System Analysis (IIASA) GGI Scenario Database, 2007 Available at: 
http://www.iiasa.ac.at/Research/GGI/DB/
The data are made available for individual, academic research purposes only and on a "as is" basis, subject to revisions without further notice. 
Commercial applications are not permitted.
}
\source{
http://www.iiasa.ac.at/web-apps/ggi/GgiDb/dsd?Action=htmlpage&page=about
}
\references{
Grubler, A., O'Neill, B., Riahi, K., Chirkov, V., Goujon, A., Kolp, P., Prommer, I., Scherbov, S. & Slentoe, E. (2006) Regional, national and spatially explicit scenarios of demographic and economic change based on SRES. Technological Forecasting and Social Change doi:10.1016/j.techfore.2006.05.023
}
\examples{
data(gridExampleData)
## maybe str(gridExampleData) ; plot(gridExampleData) ...
}
\keyword{datasets}
