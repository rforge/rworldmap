\name{addMapLegendBoxes}
\alias{addMapLegendBoxes}
\title{Add a legend of coloured boxes to a map}
\description{
Creates a colour box legend, showing the range of colours and the values the 
colours correspond to. This works well for categorical data with relatively few 
categories.
}
\usage{
addMapLegendBoxes(
                    cutVector=""    # the categories or breaks used in the map
                    ,colourVector = "" #colours used in the map
                    ,x='bottomleft'  #positioning of legend e.g. 'bottomleft', 'topright'
                    ,horiz=FALSE
                    ,title="category"
                    ,cex=1 #cex controls font size
                    ,pt.cex=2 #pt.cex controls size of colour boxes
                    ,col="gray" #boundary of boxes
                    ,bg="white" #legend background
                    ,legendText="" #if this is left as empty then the cut vector is used
                    
                    ,plottedData=""               #not used yet but maybe in future
                    ,catMethod="pretty"           #not used yet but maybe in future
                    ,colourPalette="heat"         #not used yet but maybe in future
                    ,sigFigs=2                    #not used yet but maybe in future
                    ,missingCountryCol="white"    #not used yet but maybe in future

                    ,... #to allow other params to be set in legend
                    )
}

\arguments{
  \item{cutVector}{the categories or breaks used in the map}
  \item{colourVector}{colours used in the map}
  
  \item{x}{positioning of legend e.g. 'bottomleft', 'topright'}
  \item{horiz}{ if TRUE horizontal legend}  
  \item{title}{ title for Legend }
  \item{cex}{}
  \item{pt.cex}{}
  \item{col}{}
  \item{bg}{}
  \item{legendText}{}  
    
  \item{plottedData}{not used yet but maybe in future}
  \item{catMethod}{not used yet but maybe in future}
  \item{colourPalette}{not used yet but maybe in future}
  \item{sigFigs}{not used yet but maybe in future}
  \item{missingCountryCol}{not used yet but maybe in future}  

  \item{\dots}{to allow other params to be set in legend}
}
\details{
This creates a legend with separate boxes of colour rather than addMapLegend() 
which creates a colour bar. This method is used as the default for categorical data.

See the examples for how to use a parameter list returned from mapping functions.

}
\value{
Adds a legend to a plot.
}

\author{Andy South}

\seealso{ addMapLegend, mapCountryData, mapGriddedData}
\examples{
#Set up the plot so the world map uses the full width.
mapDevice() 
#map example categorical data with no legend              
mapParams <- mapCountryData(nameColumnToPlot='REGION'
              , catMethod='categorical'
              , addLegend='FALSE' 
              )
              
#add default legend using the same parameters as mapCountryData               
do.call( addMapLegendBoxes, c( mapParams))

#adding a modified legend by specifying extra parameters               
do.call( addMapLegendBoxes, c(mapParams,x='bottom',horiz=TRUE,title="Region"))

#user defined map colour sceme              
mapParams <- mapCountryData(nameColumnToPlot='REGION'
              , catMethod='categorical'
              , addLegend='FALSE'
              , colourPalette=c('white','green','red','yellow','blue','black') 
              )
#changing legendText
mapParams$legendText <- c('antarctic','africa','oceania','americas','s.asia','eurasia')              
do.call( addMapLegendBoxes, c(mapParams,x='bottom',title="Region",horiz=TRUE))

#or this way
#do.call( addMapLegendBoxes, c(mapParams,list(legendText=c('antarctic','africa','oceania','americas','s.asia','eurasia'),x='bottom',title="Region",horiz=TRUE)))


}
\keyword{ aplot }
