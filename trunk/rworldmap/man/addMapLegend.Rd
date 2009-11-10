\name{addMapLegend}
\alias{addMapLegend}
\title{Add a legend to a map}
\description{
Creates a colour bar legend, showing the range of colours and the values the colours correspond to.
Relies heavily on image.plot() from the package fields.
For simple use, simply use addLegend=TRUE in a rworldmap map function.
Or users can call addMapLegend seperately to fine tune the legend.
The user should insure that data, catMethod,numCats and colourPalette match the values used in the plot. 
The legend is designed to be useful for the variety of classification methods that exist.
}
\usage{
addMapLegend(
                     colourVector=""
                    ,cutVector=""
                    
                    ,legendLabels="limits"              #style of bar labels 'all','none','limits'
                    ,labelFontSize=1                    #cex.axis by another name
                    ,legendWidth=1.2                    #Width in characters of the legend
                    ,legendShrink=0.9                   #Shrinks the legend, lengthwise
                    ,legendMar=3                       #shifts the legend upwards when horiz, measured in characters.
                    ,horizontal=TRUE                    #orientation
                    ,legendArgs=NULL                   #allows a title above legend
                    ,tcl=-.5                            #as per par(tcl=) tick length par default = -.5
                    ,mgp=c(3,1,0)                       #as per par(mgp=) margin position par default= c(3,1,0)
                    ,sigFigs=2                    #controls how numbers get rounded
                    ,digits=3                           #controls how numbers get formatted into neater numbers.
                    ,legendIntervals='page'       #page" or "data"."page"=intervals equal on page, "data"= equal in data units
                    
                    ,plottedData=""               #not used yet but maybe in future
                    ,catMethod="pretty"           #not used yet but maybe in future
                    ,colourPalette="heat"         #not used yet but maybe in future
                    #,missingCountryCol="white"    #not used yet but maybe in future       
                   )
}
\arguments{
  \item{cutVector}{the categories or breaks used in the map}
  \item{colourVector}{colours used in the map}  

  \item{legendLabels}{Controls the style of the labels on the legend. Choose "none" for no labels, "limits" for the two end values, and "all" to show all the break values if they fit.}
  \item{labelFontSize}{Controls font size of the labels. A multiplier, so use 2 to double the size, 0.5 to halve it, etc.}  
  \item{legendWidth}{Controls the width of the colour bar.}
  \item{legendShrink}{Controls the length of the colour bar. 1 means full width of the plot. }
  \item{legendMar}{Moves the legend away from the side of the plot. Measured in character widths.}
  \item{horizontal}{If TRUE the legend is horizontal, if FALSE, vertical.}
  \item{legendArgs}{For producing titles and labels. A list of arguments to be passed to mtext.}
  \item{tcl}{Controls the length of the tick marks.Useful when labelFontSize is changed.}
  \item{mgp}{Numeric vector length 3. The second element controls the distance between labels and the axis. Useful when labelFontSize is changed.}
  \item{sigFigs}{The number of significant figures for legend labels.}
  \item{digits}{An argument to the formatting of the labels} 
  \item{legendIntervals}{"page" or "data". Controls the division of the colour bar, "page" sets the intervals equal on the page, "data" sets them to be equal in the units of the data.}
  
  \item{plottedData}{Numeric vector. The data which was plotted}
  \item{catMethod}{Controls the classification scheme. Usual options of "pretty", "quantiles", "fixedWidth", "logFixedWidth" or a vector of breaks.}
  \item{colourPalette}{Controls the colour scheme. Usual options of "palette","heat","topo",...}

}
\details{
The default legend is a horizontal colour bar, with labels only at the extremes.

Can use a parameter list returned from mapping functions, e.g. mapCountryData().
mapCountryData(addLegend=TRUE)
produces same results as:
mapParams <- mapCountryData(addLegend=FALSE)
do.call(addMapLegend, mapParams)

Using the following allows the modification of the legend :
mapParams <- mapCountryData(addLegend=FALSE)
do.call(addMapLegend, c(mapParams, legendLabels="all", legendWidth=0.5))

}
\value{
Adds a legend to a plot.
}

\note{
Can have the unintentional effect of modifying graphical parameters, e.g. mfcol reverts to mfrow.
}

\author{Andy South & Matthew Staines}

\seealso{ mapCountryData, mapGriddedData, image.plot}
\examples{
#Set up the plot so the world map uses the full width.
mapDevice() 
#join eaxmple data to a map  
data("dFexampleCountryData",envir=environment())
sPDF <- joinCountryData2Map(dFexampleCountryData
              , joinCode = "ISO3"
              , nameJoinColumn = "ISO3V10"
              , projection = "none" 
              )
#map the data with no legend              
mapParams <- mapCountryData( sPDF
              , nameColumnToPlot="BIODIVERSITY"
              , addLegend='FALSE' 
              )
              
#add a modified legend using the same initial parameters as mapCountryData               
do.call( addMapLegend, c( mapParams
                        , legendLabels="all"
                        , legendWidth=0.5
                        ))



}
\keyword{ aplot }
