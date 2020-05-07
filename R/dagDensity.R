
#' Density of a Directed Acyclic Graph (DAG)
#'
#' A Directed Acyclic Graph (DAG) is a hierarchical structure of 3 levels of
#' vertices: the source set, the intermediaries set and the targets set. Given
#' that edges outgoing from targets and incoming to sources cannot exist, this
#' metric compute a relevant graph density for DAGs?
#'
#' @param g an igraph object; a directed acycic graph, with is_dag=TRUE.
#'
#' @return a numeric between 0 and 1.
#' @export
#'
#' @importFrom igraph is_dag
#' @importFrom igraph V
#' @importFrom igraph E
#' @examples
#' library(igraph)
#' g = make_tree(10)
#' edge_density(g)
#' edge_density(as.undirected(g))
#' is_dag(g)
#' dagDensity(g)
#'
dagDensity = function(g)
{
  if (!is_dag(g))
    stop("g require a Directed Acyclic Graph (DAG)")
  # Disriminate auto 3 vertices levels: sources, intermediaries, targets
  P = length(V(g)[!degree(g, mode="in")])
  I = length(V(g)[degree(g, mode="in") & degree(g, mode="out")])
  D = length(V(g)[!degree(g, mode="out")])
  # Comute density
  maxE = P*I + P*D + I*D + I*(I-1)
  length(E(g)) / maxE
}
