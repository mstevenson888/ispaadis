\name{isa.epicurve}

\alias{isa.epicurve}

\title{
Epidemic curve
}

\description{
Computes the cumulative number of locations predicted to experience an event as a function of time, suitable for plotting as an epidemic curve.
}

\usage{
isa.epicurve(dat, model, stats = FALSE, type = "cumulative", 
   quant = c(0.25, 0.75))
}

\arguments{
  \item{dat}{a three column data frame listing details for each iteration of the model. Format will vary depending on the model used. See details, below.}
  \item{model}{string indicating the model output to be analysed: \code{aadis} or \code{isp}.}
  \item{stats}{logical, if \code{TRUE} descriptive statistics of the event counts for each time period are calculated. If \code{FALSE} the absolute event counts by time (as rows) and iteration (columns) are reported.}
  \item{type}{string indicating the type of analysis to performed: \code{cumulative} returns the cumulative number of events by time, \code{frequency} returns the incident number of events by time.}
  \item{quant}{vector of length two, defines the quantiles to be computed. Only used if \code{stats = TRUE}.}
}

\examples{
## Load data:
data(isa.act)
data(isa.isim)

## Details of observed epidemic:
act.df <- isa.act[, c(1,2,5,6)]
act.df[,3:4] <- act.df[,3:4] / 1000

act.df <- data.frame(table(act.df$time))
act.df[,1] <- as.numeric(act.df[,1])
names(act.df) <- c("day", "n")

## Set boundaries of study area:
xlim <- c(173,273)
ylim <- c(340,440)

## Details of premises predicted to become infected by InterSpread Plus:
inf.df <- isa.isim$inf[, c(1,2,5,6)]
inf.df[,3:4] <- inf.df[,3:4] / 1000

## Compare actual epidemic curve with that predicted by InterSpread Plus:
fmd.pred <- isa.epicurve(inf.df, model = "isp", stats = FALSE, 
   type = "cumulative", quant = c(0.25, 0.75))

## Plot results:
plot(c(0,60), c(0,60), type = "n", xlab = "Simulation day", ylab = "Cumulative 
   number of infected premises")
for(i in 1:9){
   lines(fmd.pred[,1], fmd.pred[,1+i], type = "s", lwd = 1)
      }
lines(act.df$day, cumsum(act.df$n), type = "s", lwd = 2, col = "red")

## Compare actual epidemic curve with median, 25th percentile and 75th 
## percentile, predicted by InterSpread Plus:
fmd.pred <- isa.epicurve(inf.df, model = "isp", stats = TRUE, 
   type = "cumulative", quant = c(0.25, 0.75))

## Plot results:
plot(c(0, 60), c(0, 60), type = "n", 
   xlab = "Simulation day", ylab = "Cumulative number of infected premises")
lines(fmd.pred[,1], fmd.pred[,4], type = "s", lwd = 2)
lines(fmd.pred[,1], fmd.pred[,5], type = "s", lwd = 1)
lines(fmd.pred[,1], fmd.pred[,6], type = "s", lwd = 1)
lines(act.df$day, cumsum(act.df$n), type = "s", lwd = 2, col = "red")

legend(x = "topleft", legend = c("Actual cumulative number IPs", 
   "Predicted cumulative IPs (median of 9 iterations)", 
   "25th and 75th prediction quartiles"), 
   col = c("red", "black", "black"), lwd = c(3, 3, 1), bty = "n")
}

