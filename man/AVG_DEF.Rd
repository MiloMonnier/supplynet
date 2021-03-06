% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efficiency.R
\name{AVG_DEF}
\alias{AVG_DEF}
\title{Aeverage Delivery Efficiency}
\usage{
AVG_DEF(g, decr.FUN = function(x) 1/x)
}
\arguments{
\item{g}{An igraph object; the supply network.}

\item{decr.FUN}{function; how the weight of a supplier in the metric depend
decrease with its rank according to its topological distance from target node.}
}
\value{
A positive numeric.
}
\description{
This efficiency metric combine both the number of producer nodes that can be
accessed by a distributor node and also the (topological) distance at which
each supply nodes is located.
}
\details{
First, inverse supply path length between all producer-distributor
pair is computed.
distributors nodes AVG_DEF
AVG_DEF is equal to the mean of individual

Based on inverse supply pat
Decreasing function set how the weight of the supplier decreases with its rank
}
\examples{
library(igraph)
g = generateSupplyNet()
N = seq(100)
## For a tree with 1 producer, AVG_DEF decreases with number of nodes
lg = lapply(N, make_tree)
eff = sapply(lg, AVG_DEF)
plot(N, eff)

}
\references{
Zhao, K., Kumar, A., & Yen, J. (2011). Achieving High Robustness in Supply
Distribution Networks by Rewiring. IEEE Transactions on Engineering Management,
58(2), 347–362.
}
