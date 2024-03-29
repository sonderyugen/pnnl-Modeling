% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mendLoadStates.R
\name{mendLoadStates}
\alias{mendLoadStates}
\title{External data name accomodation // can be modified to accomodate different naming protocols regarding initial carbon pool state values}
\usage{
mendLoadStates(st, sts)
}
\arguments{
\item{st}{// raw external data}

\item{sts}{// placeholder state values via analytical steady state approximation}
}
\value{
// state matrix
}
\description{
External data name accomodation // can be modified to accomodate different naming protocols regarding initial carbon pool state values
}
\examples{
varName <- mendLoadStates(st, sts)

}
