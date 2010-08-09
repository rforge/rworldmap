\name{mapPolys}
\alias{mapPolys}
\title{Map country-level data.}
\description{
  Draw a map of country-level data, allowing countries to be coloured, from an object created in joinCountryData2Map().
}
\usage{
mapPolys(
                           mapToPlot = "",
                           nameColumnToPlot = "",
                           
                           numCats = 7, # *may be overridden by catMethod
                           xlim=c(-160,160),
                           ylim=c(-80,90),
                           mapRegion = "world",   #sets map extents, overrides xlim, ylim
                           catMethod="quantiles",   #any vector defining breaks, "fixedWidth","quantiles","logFixedWidth"
                           colourPalette= "heat", #"heat","white2Black","topo","palette" for current palette
                           addLegend=TRUE,
                           borderCol = 'grey',
                           mapTitle = 'columnName', #this sets it to the name of the column, any other string can be passed too
                           oceanCol=NA,
                           aspect=1,
                           missingCountryCol=NA,
                           add=FALSE                                                    
                           ) 
}
\arguments{
  \item{mapToPlot}{ a spatial polygons dataframe from joinCountryData2Map() containing country polygons and data, if none specified an internal example data is used }
  \item{nameColumnToPlot}{ name of column containing the data you want to plot }
  \item{numCats}{ number of categories to put the data in, may be modified if this number is incompatible with the catMethod chosen}
  \item{xlim}{ map extents c(west,east), can be overidden by mapRegion }
  \item{ylim}{ map extents c(south,north), can be overidden by mapRegion }
  \item{mapRegion}{ a country name from getMap()[['NAME']] or 'world','africa','oceania','eurasia','uk' sets map extents, overrides xlim,ylim }
  \item{catMethod}{ method for categorisation of data "pretty", "fixedWidth", "diverging", "logfixedWidth","quantiles","categorical", or a numeric vector defining breaks }
  \item{colourPalette}{ string describing the colour palette to use, choice of:
    \enumerate{
               \item{}"palette" for the current palette 
               \item{}a vector of valid colours, e.g. =c('red','white','blue') or output from RColourBrewer 
               \item{}one of "heat","diverging","white2Black","black2White","topo","rainbow","terrain","negpos8","negpos9"
               }}
  \item{addLegend}{ whether to add a legend or not }
  \item{borderCol}{ the colour for country borders  }
  \item{mapTitle}{ title to add to the map, any string or 'columnName' to set it to the name of the data column  }
  \item{oceanCol}{a colour for the ocean}
  \item{aspect}{aspect for the map, defaults to 1, if set to 'variable' uses same method as plot.Spatial in sp }
  \item{missingCountryCol}{a colour for missing countries}
  \item{add}{whether to add this map on top of an existing map, TRUE/FALSE}
   
  
}

\section{Warning}{will generate unhelpful errors in data categorisation if inappropriate 
options are chosen, e.g. with catMethod:Quantiles if numCats too high so that unique breaks cannot be defined.
}

\details{
Certain catMethod and colourPalette options go well together. e.g. "diverging" and "diverging", 
"categorical" and "rainbow" 

There are two styles of legend available. 
If catMethod='categorical' or the packages fields and spam are not installed a simple legend with coloured boxes is created.
Otherwise a colour bar legend is created. Finer control can be achieved by \code{\link{addMapLegendBoxes}} 
or \code{\link{addMapLegend}} repectively. 
}

\value{
invisibly returns a list containing the data and main options used for the map, 
the list can be passed to \code{\link{addMapLegend}} or \code{\link{addMapLegendBoxes}}
along with additional options to allow greater flexibility in legend creation.
}

\author{ andy south }

\seealso{ joinData2Map, classInt, RColorBrewer }

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

\keyword{ aplot }

