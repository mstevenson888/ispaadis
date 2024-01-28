\name{isa.isim}

\docType{data}

\alias{isa.isim}

\title{Details from an InterSpread Plus simulation}

\description{
This data set provides details from a typical InterSpread Plus simulation.
}

\usage{data(isa.isim)}

\format{
A list containing the following data frames:

\describe{
\item{dep}{a nine column data frame listing the following (in order): \code{itno} iteration number, \code{time} time of event, \code{id} farm identifier, \code{type} depopulation resource, \code{xcoord} easting coordinate, \code{ycoord} northing coordinate, \code{cattle} cattle numbers, \code{other} other stock numbers, \code{pigs} pig numbers.}

\item{det}{a six column data frame listing the following (in order): \code{itno} iteration number, \code{time} time of event, \code{id} farm identifier, \code{type} detection resource, \code{xcoord} easting coordinate, \code{ycoord} northing coordinate.}

\item{inf}{a six column data frame listing the following (in order): \code{itno} iteration number, \code{time} time of event, \code{id} farm identifier, \code{type} infection mechanism, \code{xcoord} easting coordinate, \code{ycoord} northing coordinate.}

\item{vac}{a nine column data frame listing the following (in order): \code{itno} iteration number, \code{time} time of event, \code{id} farm identifier, \code{type} vaccination resource, \code{xcoord} easting coordinate, \code{ycoord} northing coordinate, \code{cattle} cattle numbers, \code{other} other stock numbers, \code{pigs} pig numbers.}
}
}

\examples{
data(isa.isim)

## Not run:
library(spatstat); library(RColorBrewer)

data(isa.boundary)
data(isa.farms)

## Create an observation window:
isa.ow <- owin(poly = as.matrix(isa.boundary[172:1,]), 
   unitname = c("metre", "metres"))

## Method for SpatialPolygonsDataFrame object:
## isa.ow <- as(as(isa.boundary, "SpatialPolygonsDataFrame"), "owin")

# Create ppp objects:
farms.ppp <- ppp(x = isa.farms$xcoord, y = isa.farms$ycoord, 
   window = isa.ow)
sim.ppp <- ppp(x = isa.isim$inf$xcoord, y = isa.isim$inf$ycoord, 
   window = isa.ow)

xylims <- rbind(range(isa.farms$xcoord), range(isa.farms$ycoord))

## Method for SpatialPolygonsDataFrame object:
## xylims <- attr(is.boundary, "bbox")

ratio <- (xylims[2,2] - xylims[2,1]) / (xylims[1,2] - xylims[1,1])

x.points <- seq(from = 180000, to = 250000, by = 10000); x.lab <- x.points/1000 
y.points <- seq(from = 360000, to = 420000, by = 10000); y.lab <- y.points/1000 

## Set the bandwidth and grid dimensions:
sigma <- 1200; dimyx = c(200, 200)

## Kernel smooth:
farms.den <- density(farms.ppp, sigma = sigma, dimyx = dimyx)
sim.den <- density(sim.ppp, sigma = sigma, dimyx = dimyx)

## The density.ppp function returns an estimate of the intensity of the 
## underlying point process, that is the expected number of points per unit 
## area (in this case, the number of points per metre). Express results as 
## the number of points per square kilometre (1 square metre = 0.000001 
## square kilometres):
farms.den$v <- farms.den$v / 0.000001
summary(as.vector(farms.den$v))
## Maximum farm density is 7 per farms per square kilometre.

## Do the same for the simulations. Note the division by 9, because simulation
## results are for 9 iterations:
sim.den$v <- sim.den$v / 0.000001
summary(as.vector(sim.den$v) / 9)

## Image plot of farm density with superimposed contour plot to identify
## areas where the density of farms predicted to become infected was 
## relatively high:
breaks <- seq(from = 0, to = 7, length = 5)
col <- brewer.pal(n = 4, name = "Blues")

par(pin = c(1 * 5, ratio * 5), omi = c(0.5,0,0,0))
plot(x = xylims[1,], y = xylims[2,], type = "n", xlab = "Easting (m)", 
   ylab = "Northing (m)", xaxt = "n", yaxt = "n")
image(x = farms.den$xcol, y = farms.den$yrow, z = t(farms.den$v), 
   zlim = c(0, 7), col = col, breaks = breaks, add = TRUE)
contour(x = sim.den$xcol, y = sim.den$yrow, z = t(sim.den$v / 9),
   levels = 0.02, col = "red", add = TRUE)
plot(isa.ow, add = TRUE)
axis(side = 1, at = x.points, labels = x.lab, tick = TRUE, cex.axis = 0.80)
axis(side = 2, at = y.points, labels = y.lab, tick = TRUE, cex.axis = 0.80)
## metre(xl = xylims[1,1], yb = xylims[2,1], xr = xylims[1,1] + 2000, 
##   yt = xylims[2,1] + 15000, lab = breaks, cols = col, shift = 0, cex = 0.75)
## End(Not run)
}

\keyword{datasets}
