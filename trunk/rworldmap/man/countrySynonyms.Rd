\name{countrySynonyms}
\alias{countrySynonyms}
\docType{data}
\title{
Synonyms of country names for each ISO 3 letter country code to enable conversion.
}
\description{
contains a variable number of synonyms (mostly English language) for each country
}
\usage{data(countrySynonyms)}
\format{
  A data frame with 281 observations on the following 10 variables.
  \describe{
    \item{\code{ID}}{a numeric vector}
    \item{\code{ISO3}}{ISO 3 letter country code}
    \item{\code{name1}}{country name - most common}
    \item{\code{name2}}{country name - alternative}
    \item{\code{name3}}{country name - alternative}
    \item{\code{name4}}{country name - alternative}
    \item{\code{name5}}{country name - alternative}
    \item{\code{name6}}{country name - alternative}
    \item{\code{name7}}{country name - alternative}
    \item{\code{name8}}{country name - alternative}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
http://www.naturalearthdata.com/
}

\examples{
data(countrySynonyms)

}
\keyword{datasets}
