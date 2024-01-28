\name{isa.act}

\docType{data}

\alias{isa.act}

\title{
Details from an outbreak of foot-and-mouth disease
}

\description{
A six column data frame listing infection dates and location details of farms infected during an outbreak of foot-and-mouth disease.
}

\usage{
data(isa.act)
}

\format{
  \describe{
  \item{itno}{iteration number.}
  \item{time}{the estimated day of infection (relative to the date of infection of the index case).}
  \item{id}{farm identifier.}
  \item{type}{farm type.}
  \item{xcoord}{easting coordinate of the farm centroid.}
  \item{ycoord}{northing coordinate of the farm centroid.}
}
}

\examples{
data(isa.act)
}

\keyword{datasets}
