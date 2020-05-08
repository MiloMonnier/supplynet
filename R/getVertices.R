
#' Get vertices from a graph
#'
#'# Vertices probababilites to be chosen can depend on 3 criterias:
#' k - Their centrality index: degree, betweenness, etc (preferential attachment)
#' t - Their topological distance from reference node(s)
#' s - Their spatial distance from reference node(s)
#' if all weigths are set to 0, its equals to random mode
#'
#' @param g igraph object.
#' @param n numeric; number of vertices to return (default: 1).
#' @param vs igraph.vs; sequence in which to choose the \code{n} vertices.
#' (default: all graph vertices).
#' @param mode character; method used to choose the \code{n} vertices , eitheer
#' 'random', 'targeted' or 'proba'. Modes 'targeted' and 'proba' can use
#' several criterias such as centrality index with \code{ct.ind.FUN}.
#' (default: 'random').
#' @param ct.ind.FUN function used to compute centrality index of the vertices sequence.
#' Used only if ci.w > 0. Default is \code{\link{betweenness}} but can also be
#' \code{\link{degree}}.
#' @param ct.ind.w exponent weighting vertices centrality index in probabilities mix.
#' If negative, less central vertices are more likely to be chosen. Default is 1.
#' @param v.dist.ref igraph.vs of reference from which to compute \code{sp.dist}
#' and \code{topo.dist}.
#' @param sp.dist.attr Name of edge attribute containing spatial distance.
#' @param sp.dist.w Exponent weighting spatial distance in probabilities mix.
#' @param topo.dist.w Exponent weighting topological distance in probabilities mix.
#'
#' @return An igraph.vs vertex or vertices sequence.
#' @export
#'
#' @importFrom igraph  V
#' @importFrom igraph  degree
#' @importFrom igraph  betweenness
#' @examples
#' library(igraph)
#' set.seed(123)
#' g = erdos.renyi.game(100, 120, 'gnm')
#' ## Get a random vertex
#' getVertices(g)
#' ## Get vertex with highest betweenness
#' v = getVertices(g, mode="targ")
#' v==V(g)[which.max(betweenness(g))]
#' ## Get vertex with highest degree
#' v = getVertices(g, mode="targeted", ct.ind.FUN=degree)
#' v==V(g)[which.max(degree(g))]
#'
getVertices = function(g,
                       n            = 1,
                       vs           = V(g),
                       mode         = c("random", "targeted", "proba"),
                       ct.ind.FUN   = betweenness,
                       ct.ind.w     = 1,
                       v.dist.ref   = V(g),
                       sp.dist.attr = "dist",
                       sp.dist.w    = 0,
                       topo.dist.w  = 0)
{
  mode = match.arg(mode)
  # if (is(vs, "character"))
  #   vs = V(g)[V(g)$name %in% vs]
  if (!is(vs, "igraph.vs"))
    stop("vs require an igraph.vs set of vertices")
  if (length(vs) <= n)
    return(vs)
  if (!is(ct.ind.FUN, "function"))
    stop("ct.ind.FUN require a function")
  if (sp.dist.w || topo.dist.w && !is(v.dist.ref, "igraph.vs"))
    stop("v.dist.ref require an igraph.vs set of vertices")

  # By default, all edges probabilities to be chosen are equal. Then, weight it
  P = rep(1, length(vs))
  if (ct.ind.w) { # Compute and add centrality index
    ci = ct.ind.FUN(g, vs) ^ ct.ind.w
    P = P * (ci / sum(ci))
  }

  # TODO Add spatial distance weight

  # TODO Add topological distance weight

  # Compute mean of disances (spatial or topological) from the vertex(ices) of reference
  # sapply(v$name, function(x) mean(getVerticesDistGeo(from=x, g=g, to=v.dist.ref, dist.geo.matrix=dist.geo.matrix)))
  P = P / sum(P)
  if (any(is.na(P))) {
    P[is.na(P)] = 0
    message("Some proba were NA, corrected with 0")
  }

  # Choosse vertices to return
  if (mode=="random") {
    sample(vs, n)
  } else if (mode=="targeted") { # Sort probabilites, and take the highests
    vs[order(P, decreasing=TRUE)][1:n]
  } else if (mode=="proba") {    # Take the highests, with more likelihood
    sample(vs, n, prob=P)
  }
}
# load("outputs/3-network_spatial_cleaned.RData")
# n            = 1
# vs           = V(g)
# mode         = "targeted"
# ct.ind.FUN   = degree
# ct.ind.w     = 1
# v.dist.ref   = V(g)
# sp.dist.attr = "dist"
# sp.dist.w    = 0
# topo.dist.w  = 0
# getVertices(g)
