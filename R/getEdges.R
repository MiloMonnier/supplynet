
#' Choose edges from an edge sequence
#'
#' Selecting graph edges randomly, or according to probabilities. Probabilities
#' can depend on 2 criterias: centrality index (betweenness, sum of extremities
#' degree, etc) and/or spatial distance. These are used only of their weight > 0.
#' By default, spatial distance is ignored (weight exponent = 0).
#'
#' @param g An igraph object.
#' @param n Number of edges returned. Default is 1.
#' @param es The edges set among which to choose. Default is all graph edges.
#' @param mode Method used to choose the edge(s): "random" (default), "targeted"
#' (e.g. take the n more central edges), or "proba" (e.g. more central and longest
#' spatial-dist edges are more likely to be chosen). *.w arguments influence targetd
#' and proba modes.
#' @param ct.ind.FUN function used to compute centrality index of the edge sequence.
#' Used only if ci.w > 0. Default is \code{\link{edge_betweenness}}.
#' @param ct.ind.w exponent weighting edge centrality index in probabilities mix.
#' If negative, less central edges are more likely to be chosen. Default is 1.
#' @param sp.dist.attr Name of edge attribute containing spatial distance.
#' If negative, shortest-distance edges are more likely to be chosen. Default is 0.
#' @param sp.dist.w Eponent weighting spatial distance in probabilities mix.
#'
#' @return An igraph.es edge or edges sequence.
#' @export
#'
#' @examples
#' library(igraph)
#' g = erdos.renyi.game(100, 120, 'gnm')
#' ## Get a random edge
#' getEdges(g)
#' ## Get the highest betweenness edge
#' getEdges(g, mode="targeted")
#' # Get 2 edges with high betweenness and low spatial distance
#' E(g)$dist = runif(length(E(g)))
#' getEdges(g, n=2, mode="proba", ct.ind.w=2, sp.dist.w=-3)
#'
getEdges = function(g,
                    n            = 1,
                    es           = E(g),
                    mode         = c("random", "targeted", "proba"),
                    ct.ind.FUN   = edge_betweenness,
                    ct.ind.w     = 1,
                    sp.dist.attr = "dist",
                    sp.dist.w    = 0)
{
  mode = match.arg(mode)
  if (!is(es, "igraph.es"))
    stop("es require an igraph.es edge sequence")
  if (mode %in% c("targeted", "proba") && ct.ind.w && !is.function(ct.ind.FUN))
    stop("ci.FUN require an edge centrality index function")

  # By default, all edges probabilities to be chosen are equal. Then, weight it
  P = rep(1, length(es))
  if (ct.ind.w) { # Compute and add centrality index
    ci = ct.ind.FUN(g, es) ^ ct.ind.w
    if (any(ci==0))
      message("at least one centrality index is equal to 0")
    P = P * (ci / sum(ci))
  }
  if (sp.dist.w) { # Add spatial distance weight to probability
    spd = get.edge.attribute(g, sp.dist.attr, es) ^ sp.dist.w
    if (any(spd==0))
      message("at least one spatial distance is equal to 0")
    P = P * (spd / sum(spd))
  }
  P = P / sum(P)

  # Choose the n edges to return
  if (length(es) < n) {
    message(paste("Requested", n, "edges but returned ", length(es)))
    es
  } else if (mode=="random") {
    sample(es, n)
  } else if (mode=="targeted") { # Sort probabilites, and take the highests
    es[order(P, decreasing=TRUE)][1:n]
  } else if (mode=="proba") {    # Take the highests, with more likelihood
    sample(es, n, prob=P)
  }
}

# rm(list=ls())
# setwd("~/Dropbox/food_circuits/")
# load("outputs/3-network_spatial_cleaned.RData")
# load("outputs/distance_matrices/road_dist.RData")
# n = 1
# es    = E(g)[1:10]
# mode  = "proba"
# ct.ind.FUN  = edge_betweenness
# ct.ind.w     = 1
# sp.dist.attr = "dist"
# sp.dist.w    = 0
