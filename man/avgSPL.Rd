% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indices.R
\name{avgSPL}
\alias{avgSPL}
\title{Average Supply-Path Length (SPL)}
\usage{
avgSPL(g)
}
\arguments{
\item{g}{an igraph object; the supply network.}
}
\value{
a positive numeric.
}
\description{
A Supply-Path is a synonym of Supply Chain. This index is equal to the average
of the shortest path length from producers to distributors.
}
\examples{
library(igraph)
g = generateSupplyNet()
avgSPL(g)

}
