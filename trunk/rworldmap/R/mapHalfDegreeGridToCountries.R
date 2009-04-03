`mapHalfDegreeGridToCountries` <-
function( inFile=""
                          , aggregateOption="sum"  #"mean","max","min"
                          , nameCountryColumn = ""
                          , suggestForFailedCodes = FALSE 
                          , projection="EqualArea"  #"none", "EqualArea" 
                          , mapResolution="low" #options low, medium, only for projection='none' initially                                                 
                          , numCats = 7 # *may be ignored by catMethod
                          , we=-160
                          , ea=160
                          , so=-80
                          , no=90 
                          , mapRegion = "world"   #sets map extents, overrides we,ea etc.
                          , catMethod="pretty"   #any vector defining breaks, "fixedWidth","quantiles"
                          , colourPalette= "heat" #"heat","white2Black","palette" for current palette
                          , addLegend=TRUE 
                         )
   { 
   
    #can fail without this
    require(sp)
                       
    #map global half degree gridded data 
    #will work on a gridascii file or a SpatialGridDataFrame or use the example data if none specified
    if ( class(inFile)=="SpatialGridDataFrame" ) 
    {
       sGDF <- inFile           
    }
    else if ( inFile == "" )
    {
       message("using example data because no file specified in mapHalfDegreeGridToCountries()\n")
       data(gridExampleData)
       sGDF <- gridExampleData # copying from the example data            
    } else if ( is.character(inFile)) 
    {           
       if ( file.exists(inFile) == F )
          {
           warning("the file: ",inFile," seems not to exist, exiting mapHalfDegreeGridToCountries()\n")
           return(F)
          }
       #reading file into a SpatialGridDataFrame   
       sGDF <- readAsciiGrid(fname=inFile)    

    } else
    {
       warning(inFile," seems not to be a valid file name or a SpatialGridDataFrame, exiting mapHalfDegreeGridToCountries()\n") 
    }
         
    #further checking grid resolution
    if ( gridparameters(sGDF)$cellsize[1]!=0.5 )
        warning(inFile," seems not to be a half degree grid, in mapHalfDegreeGridToCountries()\n")
                         
     #aggregate the data to countries (pass the sGDF because already read in above)
     dF <- aggregateHalfDegreeGridToCountries(inFile=sGDF, aggregateOption=aggregateOption)
     
     #call map plotting function
     mapCountryData( dF
                          #, nameColumnToPlot = aggregateOption
                          , nameColumnToPlot = names(dF)[2]
                          , joinCode = "UN" #options "ISO2","ISO3","FIPS","NAME", "UN" = numeric codes
                          , nameJoinColumn = "UN"
                          , nameCountryColumn = "" #there is no country column from gridascii data
                          , suggestForFailedCodes = suggestForFailedCodes 
                          , projection=projection  #"none", "EqualArea" 
                          , mapResolution=mapResolution #options low, medium, only for projection='none' initially                                                
                          , numCats = numCats # *may be ignored by catMethod
                          , we=we
                          , ea=ea
                          , so=so
                          , no=no 
                          , mapRegion = mapRegion
                          , catMethod=catMethod   #any vector defining breaks, "fixedWidth","quantiles"
                          , colourPalette = colourPalette
                          , addLegend=addLegend   
                          )      
                         
                         
    } #end of mapHalfDegreeGridToCountries()

