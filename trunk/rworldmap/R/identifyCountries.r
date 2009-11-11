identifyCountries <- function(dF=""
                             ,nameCountryColumn="NAME"
                             ,nameX="LON"
                             ,nameY="LAT"
                             ,nameColumnToPlot=""
                             ,plotSelected=FALSE
                             ,...){

#! ANDY TO DO 11/11/09
#add checks that the input parameters are valid


#also possible to get centroids from a sPDF
if (class(dF)=="SpatialPolygonsDataFrame")
   {
    centroidCoords <- coordinates(getMap())
    #within this function just need the dF bit of the sPDF
    dF <- dF@data
    #adding extra attribute columns to contain centroids (even though such columns may already be there)
    dF[['nameX']] <- centroidCoords[,1]
    dF[['nameY']] <- centroidCoords[,2]    
    nameX <- 'nameX'
    nameY <- 'nameY'    
   } else
#this assumes that the dF already has columns for lat & lon
if (class(dF)=="data.frame")
   {
    #this assumes that nameX, nameY & nameCountryColumn columns have been passed correctly

   } else
   {
    #if no object is passed use the internal map
    dF <- getMap()@data
    nameX <- 'LON'
    nameY <- 'LAT'    
   }

labels <- dF[[nameCountryColumn]]

#if an attribute column name is passed paste it's value onto end of country label
if ( nameColumnToPlot != "" ) labels <- paste(labels,dF[[nameColumnToPlot]]) 

selectedCountryIndices <- identify(x=dF[[nameX]], y=dF[[nameY]], labels=labels,...)

#allowing plotting of the boundaries of the selected countries
#this is really just an initial test of something I may develop further later
#!this will only work if the internal or a passed map is used
#!!use dF2
if (plotSelected) plot(getMap()[selectedCountryIndices,],border='blue',add=TRUE)

#return the indices, may be useful later
invisible(selectedCountryIndices)
                           
} #end of identifyCountries