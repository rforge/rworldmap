\name{joinData2Map}
\alias{joinData2Map}
\title{ Joins user polygon attribute data to a map }
\description{
Joins user polygon attribute data to a map of polygon boundaries. 
The map can either be one stored in the package or provided by the user.  
Returns a spatialPolygonsDataFrame ready for plotting using \code{\link{mapPolys}}. 
Reports join successes and failures.
}
\usage{

joinData2Map(dF
        , nameMap = ""
        , nameJoinIDMap = "ISO3"
        #, joinCode = "ISO3" #options "ISO2","ISO3","FIPS","NAME","UN"
        , nameJoinColumnData = "ISO3V10"
        , nameNameColumnData = "Country"
        , suggestForFailedCodes = FALSE 
        , projection="none"  #options "none", "EqualArea"
        , mapResolution="low" #options low, medium, only for projection='none' initially
        , verbose = FALSE #if set to FALSE it doesn't print progress messages to console                         
        )
}

\arguments{
  \item{dF}{ R data frame with at least one column of polygon IDs and one column of data }
  \item{nameMap}{ the map to join the attribute data too  }  
  \item{nameJoinIDMap}{ the name of the joinIDs in the map }
  \item{nameJoinColumnData}{ name of column in the data containing country referencing }
  \item{nameNameColumnData}{ optional name of column in the data containing polygon names (used in reporting of success/failure) }
  \item{suggestForFailedCodes}{ NOT YET ENABLED T/F whether you want system to suggest for failed codes  }
  \item{projection}{ which map projection to use, options 'none', 'EqualArea' latter uses Mollweide  }
  \item{mapResolution}{resolution of the borders in the internal map, only for projection='none' : options 'low', 'medium' }
  \item{verbose}{if set to FALSE progress messages to console are restricted}
  }
\details{
Joins user polygon attribute data provided in a 'data frame' to a map of polygon boundaries. 
The map can either be one stored in the package or provided by the user.  
Returns a spatialPolygonsDataFrame ready for plotting using \code{\link{mapPolys}}. 
Reports join successes and failures.

The user specifies the name of the column in their data containing polygon referencing.

The user can choose from different internal map resolutions and projections. 
Uses the function \code{\link{getMap}} to retrieve the map.

}
\value{
An R 'SpatialPolygonsDataFrame' [package "sp"] object with the data joined to it 
}

\author{ andy south }


\seealso{ \code{\link{mapPolys}}, \code{\link{getMap}} }
\examples{
data("countryExData",envir=environment(),package="rworldmap")

sPDF <- joinData2Map(countryExData
              , getMap()
              , nameJoinIDMap = "ISO3"
              , nameJoinColumnData = "ISO3V10"
              , projection = "none" 
              )
mapPolys( sPDF
              , nameColumnToPlot="BIODIVERSITY" 
              )

}

\keyword{ dplot }

