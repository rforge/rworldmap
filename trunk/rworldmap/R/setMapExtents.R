`setMapExtents` <-
function(mapRegion='world')
   {  
    #sets map region from names passed to it
    #by returning a data frame containing wesn
    
    listMapRegions=c('eurasia','africa','latin america','uk','oceania','asia')
    if ( mapRegion == 'world' )
       {}else
    if ( mapRegion == 'eurasia' | mapRegion == 'Eurasia' )#1
       {we=-20;   ea=110;   so=20;   no=70} else 
    if ( mapRegion == 'africa' | mapRegion == 'Africa' )#2
       {we=-20;   ea=80;    so=-30;   no=30} else 
    if ( mapRegion == 'latin america' | mapRegion == 'Latin America' )#3
       {we=-110;   ea=-40;    so=-70;   no=20} else 
    if ( mapRegion == 'uk' | mapRegion == 'UK' )#4
       {we=-10;   ea=5;    so=50;   no=70} else                  
    if ( mapRegion == 'oceania' | mapRegion == 'Oceania' )#5
       {we=50;   ea=180;    so=-50;   no=0} else 
    if ( mapRegion == 'asia' | mapRegion == 'Asia' )#6
       {we=60;   ea=140;    so=-15;   no=55} else 
    cat("The mapRegion you specified(",mapRegion,") is not known, must be one of : ",listMapRegions,"plotting whole world")         

    dFmapExtents <- data.frame(we=we, ea=ea, so=so, no=no )
    
    return(dFmapExtents)
    
    } #end of setMapExtents function

