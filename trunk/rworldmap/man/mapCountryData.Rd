\name{mapCountryData}
\alias{mapCountryData}
\title{Map country-level data.}
\description{
  Draw a map of country-level data from either a text file or a data frame
  by associating rows of the data with countries by using standard country codes.
}
\usage{
mapCountryData(inFile = "", nameColumnToPlot = "", joinCode = "ISO3",
               nameJoinColumn = "ISO3V10", nameCountryColumn = "Country",
               suggestForFailedCodes = FALSE, projection="none",
               mapResolution="low", numCats = 7,
               we=-160, ea=160, so=-80, no=90,
               mapRegion = "world",
               catMethod="pretty", colourPalette= "heat", addLegend=TRUE,
               textFileType = 'csv', verbose = FALSE,
               countryBorderCol = 'grey', mapTitle = 'columnName')   
}
\arguments{
  \item{inFile}{ either a text file or a R data frame containing data referenced by country codes, if none specified an internal example data is used }
  \item{nameColumnToPlot}{ name of column containing the data you want to plot }
  \item{joinCode}{ how countries are referenced - options are "ISO2","ISO3","FIPS","NAME", "UN" = numeric codes }
  \item{nameJoinColumn}{ name of column containing country reference code }
  \item{nameCountryColumn}{ optional name of column containing country names (used in reporting of success/failure) }
  \item{suggestForFailedCodes}{ T/F - whether you want system to suggest for failed codes NOT YET WORKING }
  \item{projection}{ map projection : 'none' or 'EqualArea' }
  \item{mapResolution}{options low, medium, only for projection='none' initially  }
  \item{numCats}{ number of categories to put the data in, may be overidden if catMethod ='pretty' }
  \item{we}{ map extents, can be overidden by mapRegion }
  \item{ea}{ map extents, can be overidden by mapRegion }
  \item{so}{ map extents, can be overidden by mapRegion }
  \item{no}{ map extents, can be overidden by mapRegion }
  \item{mapRegion}{ 'world','africa','oceania','eurasia','uk' sets map extents, overrides we,ea etc. }
  \item{catMethod}{ method for categorisation of data "pretty", "fixedWidth", "logfixedWidth","quantiles", or a numeric vector defining breaks }
  \item{colourPalette}{ "heat","white2Black","topo","palette":for current palette }
  \item{addLegend}{ whether to add a legend or not }
  \item{textFileType}{ text file delimiter: 'csv' for comma, anything else for space or tab  }
  \item{verbose}{ whether to print succeded & failed countries to console  }
  \item{countryBorderCol}{ the colour for country borders  }
  \item{mapTitle}{ title to add to the map, any string or 'columnName' to set it to the name of the data column  }
}

\section{Warning}{will generate unhelpful errors in data categorisation if inappropriate 
options are chosen, e.g. with catMethod:Quantiles if numCats too high so that unique breaks cannot be defined.
}

\details{
There are two styles of legend available. The first is produced \code{legend},
the second by \code{addMapLegend}. The second style is used when the fields and spam packages are installed.
}

\author{ andy south }

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
mapCountryData()
data("dFexampleCountryData",envir=environment())
mapCountryData( dFexampleCountryData
              , nameColumnToPlot="BIODIVERSITY"
              , joinCode = "ISO3"
              , nameJoinColumn = "ISO3V10"
              , projection = "none" 
              )

}

\keyword{ aplot }

