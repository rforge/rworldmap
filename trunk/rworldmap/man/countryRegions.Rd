\name{countryRegions}
\alias{countryRegions}
\docType{data}
\title{Regional Classification Table}
\description{
A number of regional classifications exist, e.g. SRES, Stern, etc.
This table can be used to find which grouping a country belongs to, given its country code.
A variety of different codes or groupings can be used.
}
\usage{data(countryRegions)}
\format{
  A data frame with 235 observations on the following 13 variables.
  \describe{
    \item{\code{Name}}{Country Name}
    \item{\code{ISO2}}{ISO 2 letter country code}
    \item{\code{FIPS}}{FIPS country code}
    \item{\code{Numeric}}{UN numeric country code}
    \item{\code{ISO3}}{ISO 3 letter country code}
    \item{\code{GEO3major}}{Global Environment Outlook GEO3 major region names}
    \item{\code{GEO3}}{Global Environment Outlook GEO3 major region names}
    \item{\code{IMAGE24}}{Image24 region names}
    \item{\code{GLOCAF}}{GLOCAF region names}
    \item{\code{Stern}}{Stern report region names}
    \item{\code{SRESmajor}}{SRES major region names}
    \item{\code{SRES}}{SRES region names}
    \item{\code{GBD}}{Global Burden of Disease GBD region names}
    \item{\code{AVOIDnumeric}}{numeric codes for AVOID regions}
    \item{\code{AVOIDname}}{AVOID regions}    
    \item{\code{Vulnerability.Name}}{country names with spaces replaced by '.'}    
    \item{\code{GIS.Country}}{ESRI country names} 
    \item{\code{CNTRY_NAME}}{more slightly different country names} 
    \item{\code{GMI_CNTRY}}{more slightly different country names} 
    \item{\code{LDC}}{UN Least Developed Countries} 
    \item{\code{SID}}{UN Small Island Developing states} 
    \item{\code{LLDC}}{UN Landlocked Developing Countries}                 
  }
}
\details{
Used by \code{\link{country2Region}} and \code{\link{mapByRegion}}.
}

\examples{
data(countryRegions,envir=environment(),package="rworldmap")
str(countryRegions)

#joining example data onto the regional classifications
data(countryExData,envir=environment(),package="rworldmap")
dF <- merge(countryExData,countryRegions,by.x='ISO3V10',by.y='ISO3')
#plotting ENVHEALTH for Least Developed Countries (LDC) against others
plot( dF$ENVHEALTH ~ dF$LDC)
points( y=dF$ENVHEALTH, x=dF$LDC)

}
\keyword{datasets}
