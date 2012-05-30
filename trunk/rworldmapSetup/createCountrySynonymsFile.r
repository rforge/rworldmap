#createCountrySynonymsFile.r

#23/5/2012 andy south
#to deal with alternative country names
#allowing a user to join dodgy names

#PERL has what I want
#http://search.cpan.org/~sbeck/Locale-Codes-3.21/lib/Locale/Codes/API.pod
#to update just copy the file Country_Codes.pm and put it in this folder

#i created a pl file to red in the pm file and output a csv I can read into R

setwd("C:\\rWorldMapData\\countryNames\\")

# to run my perl script from R
#it reads in data from pm file & outputs it to a txt file
shell("perl - w rwmCountrySynonyms.pl")

inFile <- "countrySynonyms.txt"

#reading in the csv files
ncol <- max(count.fields(inFile, sep = "\t"))

#natural earth uses ISO_A3 and they are upper case
countrySynonyms <- read.table(inFile,sep='\t', as.is=TRUE, fill=TRUE, header = FALSE
                          ,quote=""
                          ,col.names=c('ID','ISO3', paste("name", seq_len(ncol-2), sep = "")) )

#now save this into the data folder of rworldmap
save(countrySynonyms, file="C://rworldmapWorkingCopy//rworldmap//data//countrySynonyms.rda")


