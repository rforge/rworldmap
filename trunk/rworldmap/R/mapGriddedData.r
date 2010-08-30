#7/5/2010 bug fixes for if catMethod of a vector of breaks is passed
#!BEWARE that this may have broken other options
#24/5/2010 fixing & tidying catMethod
mapGriddedData <- function(
                           dataset = ""
                         , nameColumnToPlot = "" # only for multi-attribute spatialGridDataframes
                         , numCats = 5  #numCategories in map, later add options for classifying
                         , catMethod="quantiles"   #any vector defining breaks or "fixedWidth","quantiles","logFixedWidth"
                         , colourPalette= "heat" #"heat","white2Black","topo","palette" for current palette
                         , xlim=c(-160,160)
                         , ylim=c(-80,90) #-55,80 works well to exclude antarctic
                         , mapRegion = "world"   #sets map extents, overrides we,ea etc.
                         , addLegend=TRUE
                         , addBorders = 'low' #options for country borders, 'low','coarse' = low or coarse resolution, 'coasts' = coasts only, 'none' or NA for none
                         , borderCol = 'grey'
                         , oceanCol=NA
                         , landCol=NA
                         , plotData=TRUE
                         , aspect=1
                         )
   {

    #browser()
    #print("test")

    functionName <- as.character(sys.call()[[1]])

    require(maptools)
    require(sp)
    
    ## filename or nothing ##
    if (class(dataset)=='character')
       {
        if (dataset=="") #if no dataset passed
           {
            #open example one
            data(gridExData,envir=environment(),package="rworldmap")
            sGDF <- get("gridExData")
           } else
            sGDF <- readAsciiGrid(dataset)
    ## matrix or array ##        
       } else if (class(dataset)=='matrix' || class(dataset)=='array')   
       {
        if ( length(dim(dataset)) == 2 )
           {
            #assume that it's just 2 dimensions
            gridVals <- data.frame(att=as.vector(dataset))
           
            #this assumes that the matrix covers the whole globe -180 to 180, -90 to 90
            gt <- GridTopology( cellcentre.offset = c(-180,-90)
                      , cellsize = c( 360/dim(dataset)[1], 180/dim(dataset)[2] )
                      , cells.dim = dim(dataset)[1:2] )
    
            sGDF <- SpatialGridDataFrame(gt, data = gridVals)
           } else
           {
            stop("the first argument to ",functionName," if a matrix or array should have 2 dimensions, yours has, ", length(dim(dataset))) 
            return(FALSE)
           }           
    ## SGDF passed ##           
       } else if (class(dataset)=='SpatialGridDataFrame')
       {   
        sGDF <- dataset
       } else
    ## !! I could add option here for dataFrame with nameX & nameY columns   
       {
        stop("the first argument to ",functionName," should be a file name, 2D array or matrix, or SpatialGridDataFrame, yours is, ", class(dataset)) 
        return(FALSE)
       } 


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

    #CLASSIFYING THE DATA
    
    #first get the raw data    

    dataCategorised <- sGDF[[attrName]]
    
    print(dataCategorised[1:10])  
    
    #cat : checking whether method is categorical, 
    #length(catMethod)==1 needed to avoid warning if a vector of breaks is passed  
    if( length(catMethod)==1 && catMethod=="categorical" )    
      {
       #if categorical, just copy the data, add an as.factor() to convert any data that aren't yet as a factor 
       dataCategorised <- as.factor( dataCategorised )
       cutVector <- levels(dataCategorised) #doesn't do cutting but is passed for use in legend
  		 #6/5/10 bug fix
       numColours <- -1 + length(levels(dataCategorised))
      }else
      #all other catMethods except categorical 
      {
       #if catMethod is not a vector of numbers 
       if(is.character(catMethod)==TRUE)
      	{	
      		cutVector <- rwmGetClassBreaks( dataCategorised, catMethod=catMethod, numCats=numCats, verbose=TRUE )
      		#6/5/10 bug fix
      		#28/7/10 removed to avoid bug with default params
          #numColours <- -1 + length(levels(dataCategorised))
          numColours <- -1 + length(cutVector)
        } else if(is.numeric(catMethod)==TRUE)
      	#if catMethod is numeric it is already a vector of breaks	
      	{
      		cutVector <- catMethod
      		#6/5/10 bug fix
      		numColours <- -1 + length(cutVector)
      	}
    	#Categorising the data, using a vector of breaks.	
    	#dataCategorised <- cut( dataCategorised, cutVector, include.lowest=TRUE) #BUG ? 28/4/10 
      
      dataCategorised <- cut( dataCategorised, cutVector, include.lowest=TRUE,labels=FALSE)
      
      #cut : labels for the levels of the resulting category.  By default,
      #    labels are constructed using ‘"(a,b]"’ interval notation.
      #    If ‘labels = FALSE’, simple integer codes are returned instead of a factor.
        	
  	  } #end of if data are not categorical
  
    #because the numColours may be modified slightly from numCats
    #numColours <- -1 + length(levels(dataCategorised))
    #numColours <- -1 + length(unique(dataCategorised))
        
    #browser()
    
    #sGDF$indexToPlot <- as.numeric( dataCategorised ) #BUG ? 28/4/10
    #sGDF$indexToPlot <- as.factor( dataCategorised ) #BUG correction? 28/4/10
    #sGDF$indexToPlot <- dataCategorised  #BUG correction? 28/4/10
    sGDF$indexToPlot <- as.numeric( as.character( dataCategorised )) #BUG ? 28/4/10
    
    colourVector <- rwmGetColours(colourPalette,numColours)

    #setting up the map plot 
    #fills in the ocean but will get overpainted by the grid data if it doesn't have NAs in the ocean
    rwmNewMapPlot(mapToPlot=sGDF,oceanCol=oceanCol,mapRegion=mapRegion,aspect=aspect,xlim=xlim,ylim=ylim)


    #to fill in any countries with NA values in the grid
    if(!is.na(landCol))
       {#plotSimpleMap(borderCol=landCol,landCol=landCol)
        plot( getMap(), add=TRUE, border=borderCol, col=landCol )}
       
    #only plot ascii data if plotData=T (allows legend to be plotted on its own by setting plotData=F)
    if (plotData)
        #image(sGDF,add=TRUE,attr='indexToPlot',col=colourVector, xaxs='i', yaxs='i' ) #xaxs=i ensures maps fill plot area
        #7/5/10 !BUG without a breaks param then image fits colours to just those cats present
        image(sGDF,add=TRUE,attr='indexToPlot',col=colourVector, xaxs='i', yaxs='i',breaks=c(0:(length(cutVector)-1) )) #xaxs=i ensures maps fill plot area


    borderOptions = c('low','coarse','coasts',NA,'','none')
    if (addBorders=='low'){
       plot( getMap(resolution='low'), add=TRUE, border=borderCol )
       } else
    if (addBorders=='coarse'){
       plot( getMap(resolution='coarse'), add=TRUE, border=borderCol )
       } else
    if (addBorders=='coasts'){
       #uses maps library
       library(maps) 
       #map( map(interior=FALSE,add=TRUE, col=borderCol ) )
       #21/1/10 to correct bug
       map(interior=FALSE,add=TRUE, col=borderCol ) 
       } else 
    if ( ! addBorders %in% borderOptions){
       warning("unrecognised addBorders = ",addBorders, "none plotted, choose one of",paste(borderOptions,""))
       }             
    
    ## adding a default legend, can be modified by calling addMapLegend() independently  
    if (addLegend){
    
      ## simpler legend for categorical data OR if you don't have packages spam or fields.
      if((length(catMethod)==1 && catMethod=="categorical") || !require("spam") || !require("fields")){
        
        #legend(x='bottomleft', legend=c(rev(levels(dataCategorised)),"no data"), pch = 22, pt.cex=2, col=borderCol,pt.bg=c(coloursForMap[numColours:1],"white"), title="category",bg="white" )
        addMapLegendBoxes(colourVector=colourVector,cutVector=cutVector,plottedData=dataCategorised,catMethod=catMethod)          
         
      }else{
        #colour bar legend based on fields package
        addMapLegend(colourVector=colourVector,cutVector=cutVector,plottedData=sGDF[[attrName]],catMethod=catMethod,colourPalette=colourPalette)   
        }
      }

    #could add title
    #!but need to set it to the filename for gridascii files,
    #!and the column name for multi-attribute sGDFs
    #if ( mapTitle == 'columnName' ) title(nameColumnToPlot)
    #else title( mapTitle )

    #returning data to be used by addMapLegend
    #invisible(list(plottedData=sGDF[[attrName]]
    #              ,catMethod=catMethod
    #              ,colourPalette=colourPalette
    #              ,numCats=numCats
    #              )
    #         ) 
             
  #invisible(list(plottedData=eval( parse(text=paste(sys.call()[[2]],"[['",attrName,"']]",sep='')))
  #invisible(list(plottedData=mapToPlot[[nameColumnToPlot]]
  invisible(list(plottedData=sGDF[[attrName]]
                ,catMethod=catMethod
                ,colourVector=colourVector
                ,cutVector=cutVector
                ,colourPalette=colourPalette
                )
           )              

    } #end of mapGriddedData()


#specific method for an ascii grid filename
#setMethod("mapGriddedData",c("character"),
#function(dataset,...){
#sGDF<-readAsciiGrid(dataset)
#!passing nameColumnToPlot isn't necessary as there's just one column in a gridascii file
#mapGriddedData(sGDF,nameColumnToPlot=names(sGDF@data),...)
#}) #end of setMethod(mapGriddedData(character))


#OLD specific method for an ascii grid filename and name of the field to plot
#!but ascii grid files only have a single layer
#!so need to modify it to make sure it copes
#setMethod("mapGriddedData",c("character","character"),
#function(dataset,nameColumnToPlot,...){
#sGDF<-readAsciiGrid(dataset)
#!passing nameColumnToPlot isn't necessary as there's just one column in a gridascii file
#mapGriddedData(sGDF,nameColumnToPlot,...)


#OLD 10/9/09 shouldn't need this because specified in method above
#if no name specified for the column to plot just use the first one
#setMethod("mapGriddedData",c("SpatialGridDataFrame"),
#function(dataset,nameColumnToPlot='missing',...){
#nameColumnToPlot <- names(sGDF)[1]
#mapGriddedData(sGDF,nameColumnToPlot=nameColumnToPlot,...)
#}) #end of setMethod(mapGriddedData(sGDF))












