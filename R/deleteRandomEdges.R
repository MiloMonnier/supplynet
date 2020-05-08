
#' Delete random edges from a supply network
#'
#' Delete \code{n} random edges contained in the \code{es} set from supply network
#' \code{g}. Deletion of bridges or supply-cut edges can be avoided.
#'
#' @param g igraph object; a supply network.
#' @param n numeric; the number of edges to delete (default: 1).
#' @param es igraph.es; the edges which can be deleted (default: all edges).
#' @param supply.cuts boolean; FALSE make impossible the removal of last incoming
#' or outgoing edges of a vertex (default: FALSE).
#' @param bridges boolean; FALSE make impossible the removal of an edge which
#' would increase the number of components in the graph.
#'
#' @seealso \code{\link{getBridges}}
#' @return an igraph object.
#' @export
#'
#' @examples
#' library(igraph)
#' ## Create supply network
#' type = rep(c("P","D"), each=3)
#' name = paste0(type, rep(1:3, 2))
#' x    = rep(1:3, 2)
#' y    = rep(2:1, each=3)
#' v = data.frame(name=name, type=type, x=x, y=y)
#' e = c("P1","D1", "P1","D2", "P2","D1", "P2","D2", "P2","D3", "P3","D3")
#' e = matrix(e, ncol=2, byrow=2)
#' g = graph_from_data_frame(e, directed=TRUE, v)
#' plot(g)
#' ## Highlight supply cuts
#' outdegi = degree(g, ends(g,E(g))[,1], mode="out")
#' indegj  = degree(g, ends(g,E(g))[,2], mode="in")
#' supply_cuts = E(g)[outdegi==1  |  indegj==1]
#' E(g)$color = "grey"
#' E(g)[supply_cuts]$color = "blue"
#' plot(g)
#' ## Highlight bridges
#' bridges = getBridges(g)
#' E(g)$color = "grey"
#' E(g)[bridges]$color = "red"
#' plot(g)
#' ## impossible to delete 2 edges without disconnecting the graph
#' g = deleteRandomEdges(g, bridges=FALSE)
#' # Fails
#' g = deleteRandomEdges(g, bridges=FALSE)
#'
deleteRandomEdges = function(g,
                             n           = 1,
                             es          = E(g),
                             supply.cuts = FALSE,
                             bridges     = TRUE)
{
  if(!is(es, "igraph.es"))
    stop("es require an igraph.es edge sequence")
  if (is.null(E(g)$name))
    E(g)$name = paste(ends(g,E(g))[,1], ends(g,E(g))[,2], sep="_")
  # Don't disconnect a vertex from its ultimate supplier or customer
  if (!supply.cuts) {
    outdegi = degree(g, ends(g,es)[,1], mode="out")
    indegj  = degree(g, ends(g,es)[,2], mode="in")
    es = es[outdegi>1  &  indegj>1]
  }
  if (!length(es)) {
    warning("Impossible to delete anymore edges")
    return(g)
  }
  # Don't delete bridges edges
  if (!bridges) {
    bridges = getBridges(g)
    es = es[!es$name %in% bridges$name]
  }
  if (!length(es)) {
    warning("Impossible to delete anymore edges")
    return(g)
  }
  # Choose an edge among the edge set left and delete it
  g = delete_edges(g, sample(es, n))
}
