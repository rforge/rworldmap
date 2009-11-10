`rwmNewMapPlot`<-
function(mapToPlot=getMap(),
         oceanCol=NA,
         mapRegion="world",
         xlim=c(-160,160),                  
         ylim=c(-80,90),
         aspect=1){

  #browser()

  ## setting map extents if a mapRegion has been specified
  if (mapRegion != "world"){
    dFwesn <- setMapExtents(mapRegion)
    xlim <- c(dFwesn$we, dFwesn$ea)
    ylim <- c(dFwesn$so, dFwesn$no)
  }
  
  #for setting region when projection is different
  #Mollweide map goes to max 17840888 
  #using estimated coord conversion rather than proper projection through spTransform   
  #can instead just query the bbox of the mapToPlot object
  if ( mapToPlot@bbox[3] > 100000 ) { 
  #if (projection=="EqualArea" || projection=="equalArea"){  
    xlim=xlim*100000
    ylim=ylim*100000
  }
  
  plot.new()

  #replicate behaviour of plot.Spatial in the sp package regarding aspect
  #if the map is unprojected the aspect is set based upon the mean y coord
  #only if region not 'world'
  if (aspect == 'variable' & mapRegion != "world")
        aspect <- ifelse(is.na(proj4string(mapToPlot)) || is.projected(mapToPlot),
            1, 1/cos((mean(ylim) * pi)/180))

  plot.window(xlim=xlim,ylim=ylim,asp=aspect)#,xaxs='i',yaxs='i')#,bg=oceanCol,xpd=NA)
  
  #rect(xlim[1],ylim[1],xlim[2],ylim[2],col=oceanCol,border=oceanCol)
  #making the rectangle as big as the whole map should ensure it fills
  rect(mapToPlot@bbox[1],mapToPlot@bbox[2],mapToPlot@bbox[3],mapToPlot@bbox[4],col=oceanCol,border=oceanCol)
}
