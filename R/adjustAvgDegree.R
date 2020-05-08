#' Adjust the average degree of a graph
#'
#' During a rewiring process, several operations such as nodes deletion
#' can make change the average degree of a graph. This function aims to
#' re-adjust the number of edges to a given value by adding or deleting edges
#' randomly.
#'
#' @param g an igraph object.
#' @param avg.degree numeric; the expected average degree of the output graph.
#' @param ... other arguments passed to \code{\link{addRandomEdges}} to
#' constrain the add of edges. For example, if we want the edges to be added
#' to long supply chains, we pass \code{v2connect=V(g)[type=="I"]}
#'
#' @return an igraph object.
#' @export
#'
#' @examples
#'
adjustAvgDegree = function(g,
                           avg.degree = 1.470968,
                           ...)
{
  expE = round(vcount(g) * avg.degree)
  while (length(E(g)) < expE)
    g = addRandomEdges(g, ...)
  while (length(E(g)) > expE)
    g = deleteRandomEdges(g, isolation=FALSE)
  return(g)
}
