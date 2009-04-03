`rwmApplyClassBreaks` <-
function( dataColumn, catMethod, numCats)
{

  dataCategorised <- dataColumn

	#STEP TWO: Choose and apply the appropriate classification.
	
	#if catMethod is a character it specifies a classification
	#currently fixedWidth, quantiles, pretty
  #these are tested in rwmGetClassBreaks() & warning given if not correct
  	
	if(is.character(catMethod)==TRUE)
	{	
		cutVector <- rwmGetClassBreaks( dataCategorised, numCats, catMethod )	
	}
	#if catMethod is numeric it is already a vector of breaks
	if(is.numeric(catMethod)==TRUE)
	{
		cutVector <- catMethod
	}
	
	#Categorising the data, using a vector of breaks.
	
	dataCategorised <- cut( dataCategorised, cutVector, include.lowest=TRUE)	
	
	return(dataCategorised)

}  #end of rwmApplyClassBreaks 

