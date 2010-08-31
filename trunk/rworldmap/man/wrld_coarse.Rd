\name{wrld_coarse}
\Rdversion{1.1}
\alias{wrld_coarse}
\docType{data}
\title{ a very coarse world map, only suitable for small global maps }
\description{
A 'SpatialPolygonsDataFrame' [package "sp"] object containing a very simplified world map. Polygons are attributed with country codes.
}
\usage{data(wrld_coarse)}
\format{
\preformatted{
Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
  ..@ data       :'data.frame':	174 obs. of  12 variables:
  .. ..$ SP_ID    : Factor w/ 174 levels "AFG","AGO","ALB",..
  .. ..$ FIPS     : Factor w/ 173 levels "AE","AF","AG",..
  .. ..$ ISO2     : Factor w/ 174 levels "AD","AE","AF",..
  .. ..$ ISO3     : Factor w/ 174 levels "AFG","AGO","ALB",..
  .. ..$ UN       : int [1:174] 4 24 8  ...
  .. ..$ NAME     : Factor w/ 174 levels "Afghanistan",..
  .. ..$ AREA     : int [1:174] 65209 124670 2740  ...
  .. ..$ POP2005  : num [1:174] 25067407 16095214 3153731 ...
  .. ..$ REGION   : int [1:174] 142 2 150 ...
  .. ..$ SUBREGION: int [1:174] 34 17 39  ...
  .. ..$ LON      : num [1:174] 65.22 17.54 20.07  ...
  .. ..$ LAT      : num [1:174] 33.7 -12.3 41.1 ...
  .. ..- attr(*, "data_types")= chr [1:12] "C" "C" "C" "C" ...
  ..@ polygons   :List of 174
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  
  ..@ plotOrder  : int [1:174] 8 135 28 166 31 23 10 66 85 6 ...
  ..@ bbox       : num [1:2, 1:2] -180 -90 180 83.6
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:2] "r1" "r2"
  .. .. ..$ : chr [1:2] "min" "max"
  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots
  .. .. ..@ projargs: chr NA
}
}
\source{
original shapefile obtained from : http://thematicmapping.org/downloads/world_borders.php
Text from the site :
"The original shapefile (world_borders.zip, 3.2 MB) was downloaded from the Mapping Hacks website: http://www.mappinghacks.com/data/

The dataset is available under a Creative Commons Attribution-Share Alike License. If you use this dataset, please provide a link to this website."

imported to R and cleaned by Roger Bivand

simplified using mapShaper (www.mapshaper.org) and reduce the size to 30 percent for faster printing


}

\examples{
data(wrld_coarse)
}
\keyword{datasets}
