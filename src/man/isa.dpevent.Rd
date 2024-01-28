\name{isa.dpevent}

\alias{isa.dpvac}

\title{
Daily probability of an event
}

\description{
Estimates the individual time period probability of an event given a finite observation period, the size of the population at risk and the proportion of the population that experience the event of interest at the end of the observation period.
}

\usage{
isa.dpevent(nday, npop, tpevent, replace = FALSE)
}

\arguments{
  \item{nday}{scalar, the length of the observation period (usually days).}
  \item{npop}{scalar, the size of the population at individuals risk.}
  \item{tpevent}{scalar, the proportion of the population that experience the event of interest at the end of the observation period.}
  \item{replace}{logical, if \code{TRUE} individuals are sampled with replacement. If \code{FALSE} (the usual situation) individuals are sampled without replacement.}
}

\value{
Scalar, the daily probability of event occurrence. 
}

\details{
In InterSpread Plus the Vaccination setting has a parameter \code{SelectionProb} which specifies the probability of a farm receiving a vaccination event per time period. In the usual situation this proportion is not known, in contrast to an estimate of the proportion of farms vaccinated over a given time period. This function is useful for converting proportion of farms vaccinated over a given time period to a daily vaccination probability.
}

\examples{
## A study area is comprised of 4,400 farms. You are told that over a 90 day
## period 80\% of the farm population will be vaccinated against foot and 
## mouth disease. What is the daily probability of a farm receiving a 
## vaccination assuming that once vaccinated, it will not be vaccinated again?

isa.dpevent(nday = 93, npop = 4400, tpevent = 0.60, replace = TRUE)

## The daily probability of a farm receiving a vaccination event is 
## 0.0064.
}
