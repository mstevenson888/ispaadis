\name{isa.boundary}

\docType{data}

\alias{isa.boundary}

\title{
Boundary of study area
}

\description{
Polygon defining the boundary of a hypothetical study area.
}

\usage{
data(isa.boundary)
}

\format{
A two column data frame with the following variables:

\describe{
  \item{xcoord}{easting coordinates of the boundary vertices.}
  \item{ycoord}{northing coordinates of the boundary vertices.}
  }
}

\examples{
data(isa.boundary)

## Not run:
library(spatstat)

## Create an observation window. isa.boundary is two column data frame with
## 173 rows. Reverse the row order and drop the last set of coordinates to 
## coerce the data into a suitable format for spatstat:
isa.ow <- owin(poly = as.matrix(isa.boundary[172:1,]), 
   unitname = c("metre", "metres"))

## Method for SpatialPolygonsDataFrame object:
## isa.ow <- as(as(isa.boundary, "SpatialPolygonsDataFrame"), "owin")

plot(isa.ow, axes = TRUE)
## End(Not run)
}

\keyword{datasets}