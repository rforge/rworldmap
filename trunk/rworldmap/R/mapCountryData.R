mapCountryData <- function(
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
                           missingCountryCol=NA                                                    
                           ){
                           
  functionName <- as.character(sys.call()[[1]])
                           
  #browser()  #n to enter the step through debugger, Q to exit
  
  require(sp)
  
  ## checking the data
  ## aug09 requires a SpatialPolygonsDataFrame
  ## or uses the example data in the maps POP2005 (i.e. doesn't need to join to countryExData)
 
 if ( class(mapToPlot)=="SpatialPolygonsDataFrame" ) {
    ## checking if there is any data in the dataFrame
    if ( length(mapToPlot@data[,1]) < 1 ){
      stop("seems to be no data in your chosen file or dataframe in ",functionName) 
      return(FALSE)
    } 
  } else if ( mapToPlot == "" ) {
    message(paste("using example data because no file specified in",functionName))
    mapToPlot <- getMap(resolution="low",projection="none")
    #data("countryExData",envir=environment(),package="rworldmap")
    #dF <- get("countryExData") # copying from the example data
    ## also setting a defsult nameColumnToPlot if it isn't set
    if ( nameColumnToPlot == "" ) nameColumnToPlot <- "POP2005" #
  } else {
    #warning(inFile," seems not to be a valid file name or data frame, exiting ",functionName,"\n")
    stop(functionName," requires a SpatialPolygonsDataFrame object created by the joinCountryData2Map() function \n")
    return(FALSE) 
  }
  
  ## check that the column name exists in the data frame
  if ( is.na(match(nameColumnToPlot, names(mapToPlot@data)) )){
    stop("your chosen nameColumnToPlot :'",nameColumnToPlot,"' seems not to exist in your data, columns = ",paste(names(mapToPlot@data),""))
    return(FALSE)
  } 
  
  ##classify data into categories   

  dataCategorised <- mapToPlot@data[[nameColumnToPlot]]
  
  #checking whether method is categorical, length(catMethod)==1 needed to avoid warning if a vector of breaks is passed  
  if( length(catMethod)==1 && catMethod=="categorical" ) #if categorical, just copy the data, add an as.factor() to convert any data that aren't yet as a factor   
    { 
      dataCategorised <- as.factor( dataCategorised )
      cutVector <- levels(dataCategorised) #doesn't do cutting but is passed for use in legend
    }else
    { 
      if(is.character(catMethod)==TRUE)
    	{	
    		cutVector <- rwmGetClassBreaks( dataCategorised, catMethod=catMethod, numCats=numCats, verbose=TRUE )
    	} else if(is.numeric(catMethod)==TRUE)
    	#if catMethod is numeric it is already a vector of breaks	
    	{
    		cutVector <- catMethod
    	}
  	#Categorising the data, using a vector of breaks.	
  	dataCategorised <- cut( dataCategorised, cutVector, include.lowest=TRUE)    	
	  } #end of if data are not categorical
 
  
  ## add extra column to map attribute data
  colNameRaw <- nameColumnToPlot
  colNameCat <- paste(colNameRaw,"categorised",sep='')    
  mapToPlot@data[[colNameCat]] <- dataCategorised     
  
  ## how many colours : numCats may be overriden (e.g. for 'pretty') 	
  numColours <- length(levels(dataCategorised))
  
  ## get vector of the colours to be used in map (length=num categories)    
  colourVector <- rwmGetColours(colourPalette,numColours)
  
  ## get numeric index of which category each datapoint is in (length = num points)  
  dataCatNums <- as.numeric(dataCategorised)
  
  #adding missing country colour
  if(!is.na(missingCountryCol)){
    #adding missing country colour as the last element
    colourVector<- c(colourVector,missingCountryCol)
    #setting all missing values to the last element
    dataCatNums[is.na(dataCatNums)]<-length(colourVector)
  }
  
  #setting up the map plot
  rwmNewMapPlot(mapToPlot,mapRegion=mapRegion,xlim=xlim,ylim=ylim,oceanCol=oceanCol,aspect=aspect)
  #plotting the map
  plot(mapToPlot,col=colourVector[dataCatNums],border=borderCol,add=TRUE)
  
  #the above might need : xaxs="i",yaxs="i") #xaxs="i" ensures maps fill plot area
   
  if (addLegend){
      
      if((length(catMethod)==1 && catMethod=="categorical") || !require("spam") || !require("fields")){
      
        # simpler legend for categorical data OR if you don't have packages spam or fields.
        addMapLegendBoxes(colourVector=colourVector,cutVector=cutVector,catMethod=catMethod) #,plottedData=dataCategorised)          
      
      }else{
        #colour bar legend based on fields package
        addMapLegend(cutVector=cutVector,colourVector=colourVector,catMethod=catMethod) # ,plottedData=mapToPlot@data[[nameColumnToPlot]],catMethod=catMethod,colourPalette=colourPalette)
      }  
  
  } #end of addLegend
  
  ## add title
  if ( mapTitle == 'columnName' ){
    title(nameColumnToPlot)
  } else {
    title( mapTitle )
  }
   
  ##29/10/09 returning parameter list that can be used by do.call(addMapLegend,*)  
  #sys.call()[[2]] gets the name of the first argument
        
  #invisible(list(plottedData=eval( parse(text=paste(sys.call()[[2]],"[['",nameColumnToPlot,"']]",sep='')))
  invisible(list(colourVector=colourVector
                ,cutVector=cutVector
                ,plottedData=mapToPlot[[nameColumnToPlot]]
                ,catMethod=catMethod
                ,colourPalette=colourPalette
                )
           ) 
  
  #failed attempt at creating something that could be directly used in addMapLegend()         
  #invisible(list(plottedData=paste("'",sys.call()[[2]],"'",sep='')
  #              ,nameColumnToPlot=paste("'",nameColumnToPlot,"'",sep='')
  #              ,catMethod=paste("'",catMethod,"'",sep='')
  #              ,colourPalette=paste("'",colourPalette,"'",sep='')
  #              ,numCats=numCats
  #              )
  #         )            
           
            
} #end of mapCountryData()

