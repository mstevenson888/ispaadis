\name{isa.move}

\docType{data}

\alias{isa.move}

\title{Movement details from an InterSpread Plus simulation}

\description{
This data set provides details of movement events generated during a typical InterSpread Plus simulation.
}

\usage{data(isa.move)}

\format{
A list containing the following data frames:

  \describe{
\item{ex1}{an eight column data frame listing the following (in order): \code{itno} iteration number, \code{time} time of event, \code{outcome} infection outcome (infected or not infected), \code{src.xcoord} easting of the location acting as the source of the movement event, \code{src.ycoord} northing of the location acting as the source of the movement event, \code{des.xcoord} easting of the destination location of the movement event, \code{src.ycoord} northing of the destination location of the movement event, \code{type} movement type.}
\item{ex2}{an eight column data frame listing the following (in order): \code{itno} iteration number, \code{time} time of event, \code{outcome} infection outcome (infected or not infected), \code{src.xcoord} easting of the location acting as the source of the movement event, \code{src.ycoord} northing of the location acting as the source of the movement event, \code{des.xcoord} easting of the destination location of the movement event, \code{src.ycoord} northing of the destination location of the movement event, \code{type} movement type.}
}
}

\examples{
data(isa.move)
}

\keyword{datasets}
