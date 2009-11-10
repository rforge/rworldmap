#! addMapLegendBoxes : still in development
`addMapLegendBoxes`<- function(
                    cutVector=""    # the categories or breaks used in the map
                    ,colourVector = "" #colours used in the map
                    ,x='bottomleft'
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
                    ){
                    
#function for categorical legend or if user doesn't have fields package

#this is how it was previously done in mapCountryData
#legend(x='bottomleft', legend=c(levels(dataCategorised),"no data"), pch = 22, pt.cex=2, col=borderCol,pt.bg=c(coloursForMap[1:numColours],missingCountryCol), title="category",bg="white" )

#!? deal with what happens if non categorical data get through

if (length(legendText)==1 && legendText=="") legendText=cutVector

legend( x=x
      , horiz=horiz
      , legend=legendText #cutVector #c(levels(plottedData),"no data")
      , pch = 22
      , cex=cex
      , pt.cex=pt.cex
      , pt.bg=c(colourVector)#,missingCountryCol) #or is missingCountryCol already added on ?
      , title=title
      , col=col
      , bg=bg
      , ...)


} #end of addMapLegendBoxes