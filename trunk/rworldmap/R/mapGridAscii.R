`mapGridAscii` <-
function( inFile = ""
                         , numCats = 5  #numCategories in map, later add options for classifying
                         , logIndex = TRUE  #16/3/09 replaced by catMethod below
                         , we=-180, ea=180, so=-90, no=90  #so=-55, no=80 works well to exclude antarctic
                         , numHistBarsPerCat = 1
                         , addTo0ForLog = 0.01
                         , mapRegion = "world"   #sets map extents, overrides we,ea etc.
                         , titleHist=""
                         , addHist=FALSE
                         , addLegend=TRUE 
                         , addCountryOutlines=TRUE
                         , plotData=TRUE 
                         , catMethod="logFixedWidth"   #any vector defining breaks or "fixedWidth","quantiles","logFixedWidth"
                         , colourPalette= "heat" #"heat","white2Black","topo","palette" for current palette
                         , nameColumnToPlot = "" # only for multi-attribute spatialGridDataframes
                         , countryBorderCol = 'grey'
                         #, projection="none" #, "EqualArea"                          
                         )
   {  
    #function to map a gridascii file or sGDF
    #needs tidying up
    
    #16/3/09
    #thinking of adding mapping of multi-attribute sGDFs
    #then maybe want to call the method something different from mapGridAscii perhaps mapGrid
    #I'm also tempted to remove the addHist
    #
    
    require(maptools)
    require(sp)
    #require(maps) #i'd like to replace the need for these map outlines
    
    #map global half degree gridded data 
    #will work on a gridascii file or a SpatialGridDataFrame or use the example data if none specified
    if ( class(inFile)=="SpatialGridDataFrame" )
    { 
       sGDF <- inFile
       
       #if the sGDF contains multiple attribute columns, decide which to plot
       if ( length(sGDF@data) == 1 ) attrName <- names(sGDF)[1] #to be able to get at data using original filename
       else if ( length(sGDF@data) > 1 && nameColumnToPlot != "" )
       {
          attrName <- nameColumnToPlot
          #!then want to check wkether this column is an attribute column in the sGDF
          
       } else if ( length(sGDF@data) > 1 && nameColumnToPlot == "" )
       {
           attrName <- names(sGDF)[1]
           message("plotting the first data column because nameColumnToPlot not specified in mapGridAscii()\n")
       }
               
                
    } else if ( inFile == "" )
    {
       message("using example data because no file specified in mapGridAscii()\n")
       data(gridExampleData)
       sGDF <- gridExampleData # copying from the example data
       attrName <- names(sGDF)[1] #to be able to get at data using original filename 
                  
    } else if ( is.character(inFile)) 
    {
       if ( !file.exists(inFile) )
          {
           warning("the file: ",inFile," seems not to exist, exiting mapGridAscii()\n")
           return(FALSE)
          }
       #reading file into a SpatialGridDataFrame   
       sGDF <- readAsciiGrid(fname=inFile)
       attrName <- names(sGDF)[1] #to be able to get at data using original filename    

    } else
    {
       warning(inFile," seems not to be a valid file name or a SpatialGridDataFrame, exiting mapGridAscii()\n") 
       return(FALSE)
    }
       
    #now set above to cope with multi-attribute grids
    #attrName <- names(sGDF)[1] #this allows to get at the data, using the original filename
    #str(sGDF[[attrName]])

    ## setting map extents if a mapRegion has been specified
    if (mapRegion != "world")
       {
        dFwesn <- setMapExtents(mapRegion)
        we=dFwesn$we;   ea=dFwesn$ea;   so=dFwesn$so;   no=dFwesn$no
       }

    #! trying to see if I can project the grid data
    #seems not to work because the SpatialGridDataFrame gets converted to a SpatialPointsDataFrame
    #gr_wrld_mollweide <- spTransform(gr_wrld, CRS("+proj=moll"))
    #if (projection=="EqualArea")
    #   {
    #    require(rgdal)
    #    sGDF <- spTransform(sGDF, CRS("+proj=moll"))
    #   }


    #if adding histogram open a new window & divide it in two
    if ( addHist )
       {
        oldPar <- par(mfrow=c(2, 1), mai=c(0.5, 0.7, 0.3, 0.3)) # bottom, left, top, right  
        on.exit(par(oldPar)) 
       }


    #CLASSIFYING THE DATA 

    sGDF$indexToPlotAsFactor <- rwmApplyClassBreaks( sGDF[[attrName]], catMethod, numCats) 
    
    #because the numColours may be modified slightly from numCats
    numColours <- length(levels(sGDF$indexToPlotAsFactor))
       
    sGDF$indexToPlot <- as.numeric( sGDF$indexToPlotAsFactor )
   
    #to check indexing for logs
    #plot(log(sGDF@data$pa2000.asc)~sGDF@data$indexToPlot)
    
    #16/3/09 to allow the colour palette to be set from outside the function
    coloursForMap <- rwmGetColours(colourPalette,numColours) #numCats)

    #only plot ascii data if plotData=T (allows legend to be plotted on its own by setting plotData=F)
    if (plotData)
        image(sGDF,attr='indexToPlot',col=coloursForMap,xlim=c(we,ea),ylim=c(so,no), xaxs='i', yaxs='i' ) #xaxs=i ensures maps fill plot area

    #adding country outlines
    if ( addCountryOutlines )
       {
        #map('world',add=TRUE,col="grey")
        data(wrld_simpl)
        plot(wrld_simpl,add=TRUE,border=countryBorderCol)
       }
    
    #add legend to map, can change this to a better legend using the fields package
    #to offer a bunch more options, including whether to use the index val or the actual values
    #legend(x='bottomleft', legend=c(0:numCats), pch = 22, col=c(0:numCats),pt.bg=c(0:numCats), title="index",bg="white" )        
    #this one does by map categories
    #legend(x='bottomleft', legend=c(0:numCats), pch = 22, col=c(0,coloursForMap),pt.bg=c(0,coloursForMap), title="index",bg="white" )

        
	if (addLegend)
    {

      	availablePackages<-.packages(all.available = TRUE)

      	if("spam" %in% availablePackages && "fields" %in% availablePackages){

      	          addMapLegend(sGDF[[attrName]],catMethod=catMethod,colourPalette=colourPalette,numCats=numCats)

      	}else{
      	      #Old style legend if you don't have spam or fields.
          	legend(x='bottomleft', legend=c(rev(levels(sGDF$indexToPlotAsFactor)),"no data"), pch = 15, col=c(coloursForMap[numColours:1],"white"), title="category",bg="white" )
        }
    }











    #could add title
    #!but need to set it to the filename for gridascii files, 
    #!and the column name for multi-attribute sGDFs
    #if ( mapTitle == 'columnName' ) title(nameColumnToPlot)
    #else title( mapTitle )


    #histogram to check distribution of index
    if ( addHist )
       {
        #16/3/09 : because numCats may now get modified above
        numCats = numColours

        if (titleHist!="") { 
        #} else if ( logIndex ) {titleHist <- paste("Log",attrName)
        } else titleHist <- attrName
           
        #set breaks for histogram
        
        #16/3/09 numHistBarsPerCat doesn't work with new method of classifying
        #thats Ok we may just set it to 1 permanently : it was dificult for people to get their head around anyway
        
        #breaksVector <- seq( from=min(dFbyCountry[[colNameRaw]],na.rm=TRUE),to= max(dFbyCountry[[colNameRaw]],na.rm=TRUE), length.out=numHistBreaks )
        numHistBreaks <- 1 + numCats * numHistBarsPerCat
        #breaksVector <- seq( from=min(sGDF[[attrName]],na.rm=TRUE),to=max(sGDF[[attrName]],na.rm=TRUE), length.out=numHistBreaks )
        breaksVector <- seq( from=0,to=numCats, length.out=numHistBreaks )
        #cols4Hist <- rep(1:numCats, each=numHistBarsPerCat)
        #16/3/09 allowing colours to be set from palette
        cols4Hist <- rep(coloursForMap, each=numHistBarsPerCat)         
        
        #******
        #** would need to subset this by the spatial extents passed to the method
        #** if I want the histogram just to display those in the zoomed map
        hist(sGDF$indexToPlot, main=titleHist, breaks=breaksVector, col=cols4Hist)
       } #end of if( addHist )
       
    } #end of mapGridAscii function

