\name{rwmGetISO3}
\alias{rwmGetISO3}

\title{
Internal function for getting the ISO3 country code for a country name synonymn.
}
\description{
Searches countrySynonyms to get the ISO3 code. If the name is not found NA is returned.
Allows joining of imperfect names to other country data.
}
\usage{
rwmGetISO3(oddName)
}

\arguments{
  \item{oddName}{ country name that user wishes to find code for}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the ISO3 code (3 letters), or NA if one is not found
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Andy South
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
rwmGetISO3("vietnam")
}

\keyword{manip}
