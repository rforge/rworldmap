`mapCountryData` <-
function( inFile = ""
                          , nameColumnToPlot = ""
                          , joinCode = "ISO3" #options "ISO2","ISO3","FIPS","NAME", "UN" = numeric codes
                          , nameJoinColumn = "ISO3V10"
                          , nameCountryColumn = "Country"
                          , suggestForFailedCodes = FALSE 
                          , projection="none" #, "EqualArea" 
                          , mapResolution="low" #options low, medium, only for projection='none' initially                        
                          , numCats = 7 # *may be overridden by catMethod
                          , we=-160
                          , ea=160
                          , so=-80
                          , no=90 
                          , mapRegion = "world"   #sets map extents, overrides we,ea etc.
                          , catMethod="pretty"   #any vector defining breaks, "fixedWidth","quantiles","logFixedWidth"
                          , colourPalette= "heat" #"heat","white2Black","topo","palette" for current palette
                          , addLegend=TRUE
                          , textFileType = 'csv' #options 'csv' or anything else will be read as space/tab delimited 'other'  
                          , verbose = FALSE #TRUE #whether to print succeded & failed countries to console
                          , countryBorderCol = 'grey'
                          , mapTitle = 'columnName' #this sets it to the name of the column, any other string can be passed too
                          )   
                          #can add bunch of other args from mapGridAscii later

   {
    #function to map country level data from a dataFrame or file (or use the example file)
    functionName <- 'mapCountryData'
    
    #failed to load data without this(&with no warning)
    require(sp)
    #10/9/08 removing requirement for rgdal 
    #require(rgdal) #for spTransform

    #checking the data    
    #will work on a file or a SpatialGridDataFrame or use the example data if none specified
    # i could put this file checking bit into its own function
      
    if ( class(inFile)=="data.frame" )
    {
       dF <- inFile    
    }
    else if ( inFile == "" )
    {
       message(paste("using example data because no file specified in",functionName,"\n"))
       data(dFexampleCountryData)
       dF <- dFexampleCountryData # copying from the example data
       #also setting a defsult nameColumnToPlot if it isn't set
       if ( nameColumnToPlot == "" ) nameColumnToPlot <- names(dFexampleCountryData)[16] #column 16 in EPI data is BIODIVERSITY                
    } else if ( is.character(inFile)) 
    {
       if ( !file.exists(inFile) )
          {
           warning("the file: ",inFile," seems not to exist, exiting ",functionName,"\n")
           return(FALSE)
          }
       #reading text file either csv or other delimiter (space or tab)
       if ( textFileType == 'csv' )   
       {dF <- read.csv( inFile, header=TRUE, as.is=TRUE,na.string='-9999' )
       } else
       {dF <- read.table( inFile, header=TRUE, as.is=TRUE,na.string='-9999' )
       }
       
    } else
    {
       warning(inFile," seems not to be a valid file name or data frame, exiting ",functionName,"\n") 
    }    

    # check that the column name exists in the data frame
    if ( is.na(match(nameColumnToPlot, names(dF)) )) 
       {warning("your chosen nameColumnToPlot :'",nameColumnToPlot,"' seems not to exist in your data, columns = ", names(dF))
        return(FALSE)} 
                     
    #checking if there is any data in the dataFrame
    if ( length(dF[,1]) < 1 )
       {warning("seems to be no data in your chosen file or dataframe in ",functionName) 
        return(FALSE)}
  
    ## setting map extents if a mapRegion has been specified
    if (mapRegion != "world")
       {
        dFwesn <- setMapExtents(mapRegion)
        we=dFwesn$we;   ea=dFwesn$ea;   so=dFwesn$so;   no=dFwesn$no
        #could set mapResolution to medium here ? but then would be tricky to alter
       }
  
    #for testing   
    #we=-160
    #ea=160
    #so=-80
    #no=90
    
    x=c(we,ea)# *100000
    y=c(so,no)# *100000

    #transforming extents if the map is projected
    #if (projection=="EqualArea")
    #   {
        #put extents into a points object to enable projection
    #    dFpoints <- SpatialPoints(data.frame(x,y), proj4string=CRS("+proj=longlat +datum=WGS84"))
    #    dFpointsProjected <- spTransform(dFpoints, CRS("+proj=moll"))
    #    x <- dFpointsProjected@coords[,1]
    #    y <- dFpointsProjected@coords[,2]     
    #   } #else #seems like this not needed once i specified projection on reading in map
       #{   
       # x=x*100000
       # y=y*100000
       #}
    
    #10/9/08 trying to use estimated coord conversion rather than proper projection through spTransform   
    #! this may be risky if users want to put points on map
    #! but should be OK if just for coloured countries
    if (projection=="EqualArea" || projection=="equalArea")
       {  
        x=x*100000
        y=y*100000
       }       
       
    
    #a) join data to the map
    #b) classify data into categories
	         
    #a) joining data to the map
    mapToPlot <- joinCountryData2Map(dF, joinCode, nameJoinColumn, nameCountryColumn, suggestForFailedCodes, projection, mapResolution, verbose)
    #if join has failed, then exit this function too, message from join should be enough
    if ( class(mapToPlot)!="SpatialPolygonsDataFrame" ) return(FALSE)

    #b) classify data into categories   
    
    #depending on the catMethod, the numCats may be overriden (e.g. for 'pretty')    
    dataCategorised <- rwmApplyClassBreaks( mapToPlot@data[[nameColumnToPlot]], catMethod, numCats)
    	
    #add extra column to map attribute data
    colNameRaw <- nameColumnToPlot
    colNameCat <- paste(colNameRaw,"categorised",sep='')    
    mapToPlot@data[[colNameCat]] <- dataCategorised     
    
   	#how many colours : numCats may be overriden (e.g. for 'pretty') 	
    numColours <- length(levels(dataCategorised))
    
    #get vector of the colours to be used in map (length=num categories)    
    coloursForMap <- rwmGetColours(colourPalette,numColours)
    
    #get numeric index of which category each datapoint is in (length = num points)  
    dataCatNums <- as.numeric(dataCategorised)
    
    #plotting the map, setting map extents if mapRegion not set to world
    if (mapRegion == "world")    if (mapRegion == "world")
    {
        plot(mapToPlot,col=coloursForMap[dataCatNums],border=countryBorderCol,xaxs="i",yaxs="i") #xaxs="i" ensures maps fill plot area
    } else 
    {   
        plot(mapToPlot,col=coloursForMap[dataCatNums],border=countryBorderCol,xlim=x,ylim=y,xaxs="i",yaxs="i")
    } 
        
    #adding a simple legend
    #! later offer option to modify this	    
    if (addLegend)
    {

    	availablePackages<-.packages(all.available = TRUE)

    	if("spam" %in% availablePackages && "fields" %in% availablePackages){

    	addMapLegend(mapToPlot@data[[nameColumnToPlot]],catMethod=catMethod,colourPalette=colourPalette,numCats=numCats)

    	}else{
    	#Old style legend if you don't have spam or fields.
        	legend(x='bottomleft', legend=c(rev(levels(dataCategorised)),"no data"), pch = 15, col=c(coloursForMap[numColours:1],"white"), title="category",bg="white" )
        	}
    }
    
    #add title
    if ( mapTitle == 'columnName' ) title(nameColumnToPlot)
    else title( mapTitle )
    
    
    } #end of mapCountryData function

