\name{getMap}
\Rdversion{1.1}
\alias{getMap}

\title{
A simple way to access maps stored in the package.
}
\description{
A simple way to access maps stored in the package.
}
\usage{
getMap(resolution = "low", projection = "none")
}
\arguments{
  \item{resolution}{options "coarse", "low" or "medium", for projection="none" only}
  \item{projection}{Options "none" or "equalArea"}
}
\value{
A SpatialPolygonsDataFrame object.
}

\author{
Barry Rowlingson & Andy South
}

\examples{
plot(getMap())
}

\keyword{ misc }

