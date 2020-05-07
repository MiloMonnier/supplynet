
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
#' @return an edge sequence of the graph defined as bridges.
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
# In tomato network, some edges can be considered as bridges even if the
# minimal degree of their extremities is >1: Interesting !
# g = delete_vertex_attr(g, "x") # Bug with x y attributes
# g = delete_vertex_attr(g, "y")
# bridges = getBridges(g)
# m = cbind(E(g)[bridges]$outdegreei, E(g)[bridges]$indegreej)
# apply(m, 1, min)

