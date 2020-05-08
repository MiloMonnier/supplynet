
#' Get edges from a graph
#'
#' Select graph edges randomly, or according to probabilities, which can depend
#' on 2 criterias: centrality index (betweenness, sum of extremities
#' degree, etc) and/or spatial distance. These are used only of their weight > 0.
#' By default, spatial distance is ignored, with an exponent of 0.
#'
#' @param g An igraph object.
#' @param n numeric; number of edges returned (default: 1).
#' @param es igraph.es; edges sequence among which to choose (default: all edges).
#' @param mode character; either 'random' or 'targeted'. Method used to choose
#' the \code{n} edge(s) (default: 'random').
#' (e.g. take the n more central edges), or "proba" (e.g. more central and longest
#' spatial-dist edges are more likely to be chosen). *.w arguments influence targetd
#' and proba modes.
#' @param ct.ind.FUN function used to compute centrality index of the edge sequence.
#' Used only if ci.w > 0. Default is \code{\link{edge_betweenness}}.
#' @param ct.ind.w numeric; exponent weighting edge centrality index in probabilities.
#' If negative, less central edges are more likely to be chosen (default: 1).
#' @param sp.dist.attr character; name of the edge attribute containing spatial
#' distance (default: 'dist').
#' @param sp.dist.w numeric; exponent weighting spatial distance in probabilities.
#' If negative, shortest-distance edges are more likely to be chosen (default: 0).
#'
#' @return An igraph.es edge(s) sequence of length \code{n}.
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

  # By default, all edges probabilities to be chosen are equal
  P = rep(1, length(es))
  # Compute and add centrality index to probability if necessary
  if (ct.ind.w) {
    ci = ct.ind.FUN(g, es) ^ ct.ind.w
    if (any(ci==0))
      message("at least one centrality index is equal to 0")
    P = P * (ci / sum(ci))
  }
  # Add spatial distance weight to probability if necessary
  if (sp.dist.w) {
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


#' Get bridges edges of a graph
#'
#' An edge in an undirected connected graph is a bridge iff removing it disconnects
#' the graph. For a disconnected undirected graph, definition is similar, a bridge
#' is an edge removing which increases number of disconnected components.
#' Function is available in igraph C version, but not in R.
#' \href{https://igraph.discourse.group/t/function-to-find-edges-which-are-bridges-in-r-igraph/154}{Solution}
#' proposed by Szabolcs Horvát.
#'
#' @param g An igraph object
#'
#' @return an igraph.es edge sequence.
#' @export
#'
#' @importFrom igraph biconnected_components
#' @importFrom igraph E
#' @examples
#' library(igraph)
#' set.seed(523)
#' g = erdos.renyi.game(100, 120, 'gnm')
#' bridges = getBridges(g)
#' E(g)$color = "gray"
#' E(g)[bridges]$color = "red"
#' plot(g, vertex.size=6, vertex.label=NA)
#'
getBridges = function(g)
{
  comps = biconnected_components(g)
  comps_edges = comps$component_edges
  bridges = comps_edges[lapply(comps_edges, length)==1]
  bridges = E(g)[unlist(bridges)]
}
