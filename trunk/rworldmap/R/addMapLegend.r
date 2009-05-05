`addMapLegend`<-
function(
                    plottedData=""                                #numeric vector... not usual inFile approach...
                    ,numCats=7                          #as per plotting
                    ,catMethod="pretty"                 #as per plotting
                    ,colourPalette="heat"               #as per plotting
                    ,legendLabels="limits"              #Controls style of legend labels.
                                                        #all try to display all of them
                                                        #none = just the colour bar
                                                        #limits = just min and max
                    ,labelFontSize=1                    #cex.axis by another name
                    ,legendWidth=1.2                    #Width in characters of the legend
                    ,legendShrink=0.9                   #Shrinks the legend, lengthwise
                    ,legendMar=3                       #shifts the legend upwards when horiz, measured in characters.
                    ,horizontal=TRUE                    #orientation
                    ,legendArgs=NULL                   #allows a title above legend
                    ,tcl=-.5                            #as per par(tcl=) tick length par default = -.5
                    ,mgp=c(3,1,0)                       #as per par(mgp=) margin position par default= c(3,1,0)
                    ,signifFigures=2                    #controls how numbers get rounded
                    ,digits=3                           #controls how numbers get formatted into neater numbers.
                    ,axisCatMethod="fixedWidth"         #controls how colour bar is divided up
                    ){
require(fields)



#Confusingly, there are two sets of break points in this function.
#The bar is divided equally, and the axis labels positioned equally stored in colourBarBreaks.

#The labels are tidied versions of the break points used in plotting.
#Tidying up is done by rounding to a few significant figures, and prettyNum.



#Ensures that addMapLegend() matches the defaults of mapCountryData().
if(identical(plottedData,"")){data("dFexampleCountryData",envir=environment());plottedData<-dFexampleCountryData$BIODIVERSITY}


#Get the breaks and colours used in the plot. These breaks are used for labelling.
if(is.numeric(catMethod)){plotBreaks<-catMethod}else{
plotBreaks<- rwmGetClassBreaks(plottedData, numCats, catMethod)
}

#Incase rwnGetClassBreaks returns a different number of breaks to the number asked for.
numCats<-length(plotBreaks)-1

#The colour bar is divided into sections. By default this is equal sections.
colourBarBreaks  <-  rwmGetClassBreaks(plottedData, numCats, axisCatMethod)

#Use the same colour schemes as mapGridAscii,mapCountryData
coloursUsed <- rwmGetColours(colourPalette, numCats)

#Simplify the plotBreaks. By rounding the numbers, it becomes easier to read.
tidyPlotBreaks <-  signif(plotBreaks,2)


#The image.plot zlim argument only requires the min and max.
zlim<-range(colourBarBreaks,na.rm=TRUE)


#Describe the axis. at means positioning
#labels means text displayed.
if(legendLabels=="limits"){
limitsIndex=c(1,length(colourBarBreaks))
axis.args=list(at=colourBarBreaks[limitsIndex],cex.axis=labelFontSize,mgp=mgp,tcl=tcl,labels=prettyNum(tidyPlotBreaks[limitsIndex],digits=digits,format="G"))
}
if(legendLabels=="none"){
axis.args=list(xaxt="n")
}
if(legendLabels=="all"){
axis.args=list(at=colourBarBreaks,cex.axis=labelFontSize,mgp=mgp,tcl=tcl,labels=prettyNum(tidyPlotBreaks,digits=digits,format="G"))
}
#The actual legend plotting command
image.plot(zlim=zlim,legend.only=TRUE,horizontal=horizontal,legend.args=legendArgs,legend.mar=legendMar,col=coloursUsed,breaks=colourBarBreaks,axis.args=axis.args,legend.width=legendWidth,legend.shrink=legendShrink)
}


