\name{joinCountryData2Map}
\alias{joinCountryData2Map}
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
, projection="none"  #options "none", "EqualArea"
, mapResolution="low" 
, verbose = FALSE   
)
}

\arguments{
  \item{dF}{ R data frame with at least one column for country reference and one column of data }
  \item{joinCode}{ how countries are referenced options "ISO2","ISO3","FIPS","NAME", "UN" = numeric codes }
  \item{nameJoinColumn}{ name of column containing country referencing }
  \item{nameCountryColumn}{ optional name of column containing country names (used in reporting of success/failure) }
  \item{suggestForFailedCodes}{ NOT YET ENABLED T/F whether you want system to suggest for failed codes  }
  \item{projection}{ which map projection to use, options 'none', 'EqualArea' latter uses Mollweide  }
  \item{mapResolution}{options low, medium, only for projection='none' initially}
  \item{verbose}{if set to FALSE it doesn't print progress messages to console}
  }
\details{
Joins data referenced by country codes to an internally stored map to enable plotting. 
The user specifies which country code their data are referenced by, and the name of the column in their data containing that referencing data.
The user can choose from different map resolutions and projections.
The function reports on how many countries successfully join to the map.
Data can then be plotted using \code{\link{mapCountryData}}.
}
\value{
An R 'SpatialPolygonsDataFrame' [package "sp"] object with the data joined to it 
}

\author{ andy south }


\seealso{ \code{\link{mapCountryData}} }
\examples{
data("countryExData",envir=environment(),package="rworldmap")

sPDF <- joinCountryData2Map(countryExData
              , joinCode = "ISO3"
              , nameJoinColumn = "ISO3V10"
              , projection = "none" 
              )
mapCountryData( sPDF
              , nameColumnToPlot="BIODIVERSITY" 
              )

}

\keyword{ dplot }

