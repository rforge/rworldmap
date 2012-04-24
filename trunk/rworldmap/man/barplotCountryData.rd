\name{barplotCountryData}
\alias{barplotCountryData}
\title{Barplot country-level data.}
\description{
Draw a barplot of country-level data, ranking the countries to allow easy comparison.
This is useful for comparing with maps created by \code{\link{mapCountryData}} and accepts many of the same arguments for categorising and colouring.
}
\usage{
barplotCountryData( dF
                   , nameColumnToPlot = ""
                   , nameCountryColumn = "NAME"                   
                   , numPanels = 4  
                   , scaleSameInPanels = FALSE
                   , main=nameColumnToPlot
                   , numCats = 5  
                   , catMethod="quantiles"                             
                   , colourPalette= "heat"
                   , addLegend=TRUE
                   , toPDF = FALSE
                   , outFile = ""
                   , ...
               )   
}
\arguments{
  \item{dF}{a dataframe containing at least one column with numeric data and one with country names or other labels }
  \item{nameColumnToPlot}{ name of column containing the data you want to plot }
  \item{nameCountryColumn}{ name of column containing country names (or other labels to be used in plot) }
  \item{numPanels}{the number of layout panels in the plot}
  \item{scaleSameInPanels}{whether to set the scale the same in each panel TRUE/FALSE, default=FALSE allowing more of the variability in the data to be viewed}
  \item{main}{title for the plot}
  \item{numCats}{ number of categories to put the data in, may be modified if this number is incompatible with the catMethod chosen}
  \item{catMethod}{ method for categorisation of data "pretty", "fixedWidth", "diverging", "logfixedWidth","quantiles","categorical", or a numeric vector defining breaks }
  \item{colourPalette}{ a string describing the colour palette to use, choice of :
    \enumerate{
               \item{}{="palette" for the current palette} 
               \item{}{a vector of valid colours, e.g. =c('red','white','blue') or output from RColourBrewer} 
               \item{}{= one of "heat","diverging","white2Black","black2White","topo","rainbow","terrain","negpos8","negpos9" }
               }}
  \item{addLegend}{ NOT YET WORKING whether to add a legend or not, TRUE/FALSE }
  \item{toPDF}{ whether to output the plot to a pdf rather than the screen, TRUE/FALSE  }
  \item{outFile}{ output filename if toPDF=TRUE  }  
  \item{\dots}{ other arguments to pass to barplot }    
  
}

\section{Warning}{will generate unhelpful errors in data categorisation if inappropriate 
options are chosen, e.g. with catMethod:Quantiles if numCats too high so that unique breaks cannot be defined.
}

\details{

Finer control can be achieved by \code{\link{addMapLegend}}. 
}

\value{
invisibly returns a list containing the data and main options used for the map, 
the list can be passed to \code{\link{addMapLegend}} or \code{\link{addMapLegendBoxes}}
along with additional options to allow greater flexibility in legend creation.
}

\author{ andy south }

\seealso{ classInt, RColorBrewer }

\examples{
#default uses popn data in the default map
barplotCountryData()


data("countryExData",envir=environment(),package="rworldmap")

barplotCountryData( countryExData
              , nameColumnToPlot="BIODIVERSITY" 
              , nameCountryColumn = "Country"
              )
              

              
}

\keyword{ aplot }
