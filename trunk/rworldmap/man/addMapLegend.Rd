\name{addMapLegend}
\alias{addMapLegend}
\title{Add a legend to a map}
\description{
Creates a simple legend, showing the range of colours and the values the colours correspond to.
For simple use, simply use addLegend=TRUE in an rworldmap map function.
Or users can call addMapLegend seperately to fine tune the legend.
The user should insure that data, catMethod,numCats and colourPalette match the values used in the plot. 
The legend is designed to be useful for the variety of classification methods that exist.
}
\usage{
addMapLegend(plottedData="", 
numCats = 7, catMethod = "pretty", colourPalette = "heat",
legendLabels = "limits", labelFontSize = 1,
legendWidth = 1.2, legendShrink = 0.9, legendMar = 3,
horizontal = TRUE, legendArgs = NULL, 
tcl = -0.5, mgp = c(3, 1, 0),  
signifFigures = 2, digits = 3,  axisCatMethod = "fixedWidth"    
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plottedData}{Numeric vector. The data which was plotted}
  \item{numCats}{Controls the number of categories for some catMethods.}
  \item{catMethod}{Controls the classification scheme. Usual options of "pretty", "quantiles", "fixedWidth", "logFixedWidth" or a vector of breaks.}
  \item{colourPalette}{Controls the colour scheme. Usual options of "palette","heat","topo",...}
  \item{legendLabels}{Controls the style of the labels on the legend. Choose "none" for no labels, "limits" for the two end values, and to show all the break values if they fit.}
  \item{labelFontSize}{Controls font size of the labels. A multiplier, so use 2 to double the size, 0.5 to halve it, etc.}  
  \item{legendWidth}{Controls the width of the colour bar.}
  \item{legendShrink}{Controls the length of the colour bar. 1 means full width of the plot. }
  \item{legendMar}{Moves the legend away from the side of the plot. Measured in character widths.}
  \item{horizontal}{If TRUE the legend is horizontal, if FALSE, vertical.}
  \item{legendArgs}{For producing titles and labels. A list of arguments to be passed to mtext.}
  \item{tcl}{Controls the length of the tick marks.Useful when labelFontSize is changed.}
  \item{mgp}{Numeric vector length 3. The second element controls the distance between labels and the axis. Useful when labelFontSize is changed.}
  \item{signifFigures}{The number of significant figures the labels are too.}
  \item{digits}{An argument to the formatting of the labels} 
  \item{axisCatMethod}{Controls the division of the colour bar. The default ensures the colours are of equal size.}
}
\details{
To make the labels easy to read, the numeric labels are shown to a only a few significant figures. 
}
\value{
Just adds a legend to a plot.
}

\author{Matthew Staines}

\seealso{mapCountryData,mapGridAscii,image.plot}
\examples{
#Set up the plot so the world map uses the full width.
oldpar<-par(mar=c(0,0,1,0),xaxs="i")   
mapCountryData(addLegend=FALSE)
addMapLegend(legendLabels="all")
par(oldpar)



}
\keyword{ aplot }
