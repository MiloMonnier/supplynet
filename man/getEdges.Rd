% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getEdges.R
\name{getEdges}
\alias{getEdges}
\title{Get edges from a graph}
\usage{
getEdges(
  g,
  n = 1,
  es = E(g),
  mode = c("random", "targeted", "proba"),
  ct.ind.FUN = edge_betweenness,
  ct.ind.w = 1,
  sp.dist.attr = "dist",
  sp.dist.w = 0
)
}
\arguments{
\item{g}{An igraph object.}

\item{n}{numeric; number of edges returned (default: 1).}

\item{es}{igraph.es; edges sequence among which to choose (default: all edges).}

\item{mode}{character; either 'random' or 'targeted'. Method used to choose
the \code{n} edge(s) (default: 'random').
(e.g. take the n more central edges), or "proba" (e.g. more central and longest
spatial-dist edges are more likely to be chosen). *.w arguments influence targetd
and proba modes.}

\item{ct.ind.FUN}{function used to compute centrality index of the edge sequence.
Used only if ci.w > 0. Default is \code{\link{edge_betweenness}}.}

\item{ct.ind.w}{numeric; exponent weighting edge centrality index in probabilities.
If negative, less central edges are more likely to be chosen (default: 1).}

\item{sp.dist.attr}{character; name of the edge attribute containing spatial
distance (default: 'dist').}

\item{sp.dist.w}{numeric; exponent weighting spatial distance in probabilities.
If negative, shortest-distance edges are more likely to be chosen (default: 0).}
}
\value{
An igraph.es edge(s) sequence of length \code{n}.
}
\description{
Select graph edges randomly, or according to probabilities, which can depend
on 2 criterias: centrality index (betweenness, sum of extremities
degree, etc) and/or spatial distance. These are used only of their weight > 0.
By default, spatial distance is ignored, with an exponent of 0.
}
\examples{
library(igraph)
g = erdos.renyi.game(100, 120, 'gnm')
## Get a random edge
getEdges(g)
## Get the highest betweenness edge
getEdges(g, mode="targeted")
# Get 2 edges with high betweenness and low spatial distance
E(g)$dist = runif(length(E(g)))
getEdges(g, n=2, mode="proba", ct.ind.w=2, sp.dist.w=-3)

}
