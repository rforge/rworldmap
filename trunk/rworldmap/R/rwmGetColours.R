`rwmGetColours` <-
function(colourPalette, numColours)
{

  #! i should add in a default for if incorrect option chosen
  # and set to palette

	if(colourPalette=="white2Black")
	{
		white2Black <- colorRampPalette(c(grey(0.8),grey(0.2)))
		coloursToUse <- white2Black(numColours)
	}

	if(colourPalette=="palette")
	{	
		coloursToUse<-palette()
	}
	
	if(colourPalette=="heat")
	{
		coloursToUse<-rev(heat.colors(numColours))	
	}

  if(colourPalette=="topo")
	{
		coloursToUse<-topo.colors(numColours)	
	}

return(coloursToUse)

} #end of rwmGetColours

