\name{wrld_simpl}
\alias{wrld_simpl}
\docType{data}
\title{ World map simple }
\description{
A 'SpatialPolygonsDataFrame' [package "sp"] object containing a simplified world map. Polygons are attributed with country codes.
}
\usage{data(wrld_simpl)}
\format{
\preformatted{
Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
  ..@ data       :'data.frame':	246 obs. of  11 variables:
  .. ..$ FIPS     : Factor w/ 244 levels "","AC","AE","AF",..
  .. ..$ ISO2     : Factor w/ 246 levels "AD","AE","AF",..
  .. ..$ ISO3     : Factor w/ 246 levels "ABW","AFG","AGO",..
  .. ..$ UN       : int [1:246] 28 12 31 8 51 24 16 32 36 48 ...
  .. ..$ NAME     : Factor w/ 246 levels "Afghanistan",..
  .. ..$ AREA     : int [1:246] 44 238174 8260 2740 2820 ...
  .. ..$ POP2005  : int [1:246] 83039 32854159 8352021 3153731 3017661 ...
  .. ..$ REGION   : int [1:246] 19 2 142 150 142 2 9 19 9 142 ...
  .. ..$ SUBREGION: int [1:246] 29 15 145 39 145 17 61 5 53 145 ...
  .. ..$ LON      : num [1:246] -61.78   2.63  47.40  20.07  44.56 ...
  .. ..$ LAT      : num [1:246] 17.1 28.2 40.4 41.1 40.5 ...
  ..@ polygons   :List of 246
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
...
  ..@ plotOrder  : int [1:246] 145 175 24 209 30 21 9 71 98 8 ...
  ..@ bbox       : num [1:2, 1:2] -180  -90  180   90
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:2] "r1" "r2"
  .. .. ..$ : chr [1:2] "min" "max"
  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots
  .. .. ..@ projargs: chr " +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
}
}
\source{
shapefile obtained from : http://thematicmapping.org/downloads/world_borders.php
Text from the site :
"The original shapefile (world_borders.zip, 3.2 MB) was downloaded from the Mapping Hacks website: http://www.mappinghacks.com/data/

The dataset is available under a Creative Commons Attribution-Share Alike License. If you use this dataset, please provide a link to this website."

imported to R and cleaned by Roger Bivand
}

\examples{
data(wrld_simpl)
}
\keyword{datasets}
