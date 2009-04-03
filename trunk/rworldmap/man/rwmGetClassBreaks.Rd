\name{rwmGetClassBreaks}
\alias{rwmGetClassBreaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Internal function to set the breaks in data classification }
\description{
Internal function to set the breaks in data classification
}
\usage{
rwmGetClassBreaks(dataColumn, numCats, catMethod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataColumn}{ the data vector to be classified }
  \item{numCats}{ number of categories to put the data in, may be overidden if catMethod ='pretty' }
  \item{catMethod}{ method for categorisation of data "pretty", "fixedWidth", "logfixedWidth","quantiles", or a numeric vector defining breaks   }
}
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
\author{ matthew staines }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
#function(dataColumn, dataCategorised, numCats, catMethod)		
#{ } #end of rwmGetClassBreaks
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
