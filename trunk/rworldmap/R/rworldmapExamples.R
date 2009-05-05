`rworldmapExamples` <-
function()
   {  
    #displays examples - i should probably put somewhere else
    
    #get the data
    data("dFexampleCountryData",envir=environment())
   
    #should check operating system for windows()
    #or just remove
    #windows()
   
    #this prompts user for keypress between plots 
    par(ask = TRUE)
   
    #1
    mapCountryData( dFexampleCountryData
                  , nameColumnToPlot="EPI"
                  , joinCode = "ISO3" #options "ISO2","ISO3","FIPS","COUNTRY"
                  , nameJoinColumn = "ISO3V10"
                  )
    mtext('mapCountryData( nameColumnToPlot="EPI", joinCode = "ISO3"
, nameJoinColumn = "ISO3V10")',line=-1)              
                  #, nameCountryColumn = "Country"
                  #, suggestForFailedCodes = FALSE 
                  #, projection="EqualArea"  #options "none", "EqualArea"                          
                  #, numCats = 7
                  #, we=-160
                  #, ea=160
                  #, so=-80
                  #, no=90 
                  #, catMethod="pretty"
                  #, colourPalette= "heat" 
                  #, addLegend=TRUE  
                  #)   
    
    #windows()
    #2
    mapCountryData( dFexampleCountryData
                  , nameColumnToPlot="ENVHEALTH"
                  , joinCode = "ISO3" #options "ISO2","ISO3","FIPS","COUNTRY"
                  , nameJoinColumn = "ISO3V10"
                  , catMethod="quantiles"
                  , numCats = 10 
                  )
    mtext('mapCountryData( nameColumnToPlot="ENVHEALTH", joinCode = "ISO3"
, nameJoinColumn = "ISO3V10", catMethod="quantiles", numCats = 10)',line=-1)                  
                  
    #windows()
    #3
    mapCountryData( dFexampleCountryData
                  , nameColumnToPlot="BIODIVERSITY"
                  , joinCode = "ISO3" #options "ISO2","ISO3","FIPS","COUNTRY"
                  , nameJoinColumn = "ISO3V10"
                  , projection = "none" 
                  )
    mtext('mapCountryData( nameColumnToPlot="BIODIVERSITY", joinCode = "ISO3"
, nameJoinColumn = "ISO3V10", projection = "none")',line=-1)              
    
    #windows() 
    #4
    mapCountryData(mapRegion="asia",projection="none")  
    mtext('mapCountryData( mapRegion="asia",projection="none")',outer=TRUE,line=-1)              

                  
    #windows()
    #aggregating gridded data to country level
    #with no file specified it uses internal example data
    #5
    mapHalfDegreeGridToCountries( )              
    mtext('mapHalfDegreeGridToCountries( )',outer=TRUE,line=-1)
    
    #windows()
    #different agggregate option
    #6
    mapHalfDegreeGridToCountries( aggregateOption="mean" )
    mtext('mapHalfDegreeGridToCountries( aggregateOption="mean" )',outer=TRUE,line=-1)
        
    #windows()
    #no parameters : default dataset
    #7
    mapGridAscii()
    mtext('mapGridAscii()')
        
    #windows()
    #adding histogram
    #8
    mapGridAscii(catMethod="logFixedWidth",addHist=TRUE)    
    mtext('mapGridAscii(catMethod="logFixedWidth",addHist=TRUE)',outer=TRUE,line=-1)
        
    #windows() 
    #9
    mapGridAscii(mapRegion="africa") #,projection="none")
    mtext('mapGridAscii(mapRegion="africa")',outer=TRUE,line=-1)
            
    #switching off prompting user for keypress between plots 
    par(ask = FALSE)     
                  
    } #end of rWorldMapExamples

