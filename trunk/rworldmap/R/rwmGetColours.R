`rwmGetColours` <-
function(colourPalette, numColours)
{

  #! i should add in a default for if incorrect option chosen
  paletteList <- c("white2Black","palette","heat","topo")
  
  #warning moved below instead
  #if( ! colourPalette %in% paletteList )
  #  {
  #   warning("colourPalette should be set to one of :",paletteList,"\nsetting to current palette as default")
  #   return( palette() )
  #  }

	if(colourPalette=="white2Black")
	{
		white2Black <- colorRampPalette(c(grey(0.8),grey(0.2)))
		coloursToUse <- white2Black(numColours)
	} else

	if(colourPalette=="palette")
	{	
		coloursToUse<-palette()
	} else
	
	if(colourPalette=="heat")
	{
		coloursToUse<-rev(heat.colors(numColours))	
	} else

  if(colourPalette=="topo")
	{
		coloursToUse<-topo.colors(numColours)	
	} else
	{	
    warning("colourPalette should be set to one of :",paletteList,"\nsetting to current palette as default") 
		coloursToUse<-palette()
	} 	
	

return(coloursToUse)

} #end of rwmGetColours

