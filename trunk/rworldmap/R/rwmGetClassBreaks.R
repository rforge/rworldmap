`rwmGetClassBreaks` <-
function(dataColumn, numCats, catMethod)
{

catMethodList <- c("fixedWidth","quantiles","pretty","logFixedWidth")

if( ! catMethod %in% catMethodList)
  {
   warning("classification method should be set to one of :",catMethodList,"\nsetting to fixedWidth as default")
   catMethod="fixedWidth"
  }


if(catMethod=="fixedWidth")
		{
			#Categorising the data, fixed width intervals
			minVal <- min(dataColumn,na.rm=TRUE)
			maxVal <- max(dataColumn,na.rm=TRUE) 
			
			cutVector <- minVal + ( ((0:numCats)/numCats) * (maxVal-minVal) )				
		}
if(catMethod=="quantiles")
		{
			#Categorising the data, using Quantiles.

    #03/04/2009 12:04:35 Matthew (whole catMethod=="quantiles" section)

   #Using Quantiles will crash if the data contains too many repeats and numCats is high.
   #The break points must be unique. The algorithm below will use numCats if it can.
   #If numCats does not produce unique breakpoints,
   #it will keep reducing the number of quantiles it will use, till unique break points are found.
   #It will also warn if the number of quantiles used was less than asked for.

    testNumCats<-numCats              #The next number of quantiles to try. starts at numCats, and decreases till unique breeakpoints are found.
    uniqueBreaksFlag<-FALSE           #Flags if unique breaks have been found. When TRUE, the while loop stops, and the current value of testNumCats is used to produce quantiles.
    while(uniqueBreaksFlag==FALSE && testNumCats > 0 )
              {
              testQuantiles<-quantile(dataColumn,probs = seq(0, 1, 1/testNumCats),na.rm=TRUE)

              if(length(testQuantiles)==length(unique(testQuantiles)))     #Are the breaks unique?
                    {
                    uniqueBreaksFlag<-TRUE   #Stop looping
                    }else{
                    testNumCats<-testNumCats-1         #Carry on looping, trying one fewer quantile.
                    }
              }
    if(testNumCats!=numCats)warning("You asked for ",numCats," quantiles, but only ",testNumCats, " were used.")  #Warning if the number of quantiles was reduced.

    cutVector <-  quantile(dataColumn, probs=seq(0,1, 1/testNumCats), na.rm=TRUE)

    }
if(catMethod=="pretty")
		{
			#Compute a sequence of about n+1 equally spaced ‘round’ values
      #which cover the range of the values in x.
      #The values are chosen so that they are 1, 2 or 5 times a power of 10.

			cutVector <- pretty(dataColumn, n=numCats)

			#03/04/2009 12:04:08   Matthew  ( pretty() warning)
			#Pretty will choose a number of categories similar to the number of categories asked for.
			#The following code warns when pretty has used a different number of breaks to that which was asked for.

      actualNumberOfBreaks<-length(cutVector)-1
      if(actualNumberOfBreaks!=numCats) warning("You asked for ",numCats," categories, but ",actualNumberOfBreaks, " were used due to pretty() classification")

		}

#16/3/09 andy this is an initial test, adds 0.01 to void problems with zeroes
if ( catMethod=="logFixedWidth") 
    {
      warning('an initial test, adds 0.01 prior to logging to avoid problems with zeroes')
    
      addTo0ForLog <- 0.01
      # to do for Logs will want to Log the data calc the CutVector then antiLog
      # to get a cutVector that can be directly applied to the data
      dataColumnLogged <- log(addTo0ForLog+dataColumn)
 			
      minVal <- min(dataColumnLogged,na.rm=TRUE)
			maxVal <- max(dataColumnLogged,na.rm=TRUE) 
			
			cutVector <- minVal + ( ((0:numCats)/numCats) * (maxVal-minVal) )	
 			
 			#antilog
      cutVector <- exp(cutVector) -  exp(log(addTo0ForLog))

      #earlier version
      #change to a log index with num categories defined by numCats
      #sGDF$indexToPlot <- as.integer( numCats * ((log(addTo0ForLog+sGDF[[attrName]]) - minAtt) / rangeAtt ))
      #16/3/09 changed as.integer to round
      #sGDF$indexToPlot <- round( numCats * ((log(addTo0ForLog+sGDF[[attrName]]) - minAtt) / rangeAtt ))
      
    }
		
return(cutVector)
		
} #end of rwmGetClassBreaks

