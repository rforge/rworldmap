\name{rworldmapExamples}
\alias{rworldmapExamples}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Example code for plot creation  }
\description{
Example code to demonstrate creation of a series of plots
}
\usage{
rworldmapExamples()
}
%- maybe also 'usage' for other objects documented here.
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

Add notes here
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
`rworldmapExamples` <-
function()
   {  
    #displays examples - i should probably put somewhere else
    
    #get the data
    data("dFexampleCountryData")   
   
    #should check operating system for this
    windows()
   
    #this prompts user for keypress between plots 
    par(ask = TRUE)
   
    #1
    mapCountryData( dFexampleCountryData
                  , nameColumnToPlot="EPI"
                  , joinCode = "ISO3" #options "ISO2","ISO3","FIPS","COUNTRY"
                  , nameJoinColumn = "ISO3V10"
                  )
    print('    mapCountryData( dFexampleCountryData
                  , nameColumnToPlot="EPI"
                  , joinCode = "ISO3" #options "ISO2","ISO3","FIPS","COUNTRY"
                  , nameJoinColumn = "ISO3V10"
                  )')              
                  #, nameCountryColumn = "Country"
                  #, suggestForFailedCodes = F 
                  #, projection="EqualArea"  #options "none", "EqualArea"                          
                  #, numCats = 7
                  #, we=-160
                  #, ea=160
                  #, so=-80
                  #, no=90 
                  #, catMethod="Pretty"
                  #, colourPalette= "heat" 
                  #, addLegend=T   
                  #)   
    
    #windows()
    #2
    mapCountryData( dFexampleCountryData
                  , nameColumnToPlot="ENVHEALTH"
                  , joinCode = "ISO3" #options "ISO2","ISO3","FIPS","COUNTRY"
                  , nameJoinColumn = "ISO3V10"
                  , catMethod="Quantiles"
                  , numCats = 10 
                  )
                  
    #windows()
    #3
    mapCountryData( dFexampleCountryData
                  , nameColumnToPlot="BIODIVERSITY"
                  , joinCode = "ISO3" #options "ISO2","ISO3","FIPS","COUNTRY"
                  , nameJoinColumn = "ISO3V10"
                  , projection = "none" 
                  )
    
    #windows() 
    #4
    mapCountryData(mapRegion="asia",projection="none")  
                  
    #windows()
    #aggregating gridded data to country level
    #with no file specified it uses internal example data
    #5
    mapHalfDegreeGridToCountries( )              
    
    #windows()
    #different agggregate option
    #6
    mapHalfDegreeGridToCountries( aggregateOption="mean" )
        
    #windows()
    #no parameters : default dataset
    #7
    mapGridAscii()
        
    #windows()
    #adding histogram
    #8
    mapGridAscii(addHist=T)    
    
    #windows() 
    #9
    mapGridAscii(mapRegion="africa") #,projection="none")
        
    #switching off prompting user for keypress between plots 
    par(ask = TRUE)     
                  
    } #end of rWorldMapExamples


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }

