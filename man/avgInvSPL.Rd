% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indices.R
\name{avgInvSPL}
\alias{avgInvSPL}
\title{Average inverse supply path length}
\usage{
avgInvSPL(g)
}
\arguments{
\item{g}{an igraph object; the supply network.}
}
\value{
a numeric between 0 and 1. 1 corresponds to all short-circuits network.
}
\description{
First, all shortest path lengths between producers and distributors are
computed. Then, their inverse are computed. This index returns the mean of
the inverses paths length (and not the inverse of the avgSPL).
}
\examples{
library(igraph)
g = generateSupplyNet()
avgInvSPL(g)

}
