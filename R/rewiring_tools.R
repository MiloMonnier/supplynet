
#' Update vertices and attributes of a graph
#'
#' First, different degrees (deg, indeg, outdeg) are computed and attached as
#' vertices attributes. Then, given these degrees, vertice type (P, I, D) is
#' deducted. Edges type is equal to the concatenation of ends vertices types.
#'
#' @param g an igraph object; the supply network.
#'
#' @return an igraph object.
#' @export
#'
#' @examples
#' library(igraph)
#' g = make_tree(10)
#' g = updateGraphAttributes(g)
#' V(g)$type
#' E(g)$type
#'
updateGraphAttributes = function(g)
{
  # Detect sources, targets and intermediaries
  V(g)$deg    = degree(g)
  V(g)$indeg  = degree(g, mode="in")
  V(g)$outdeg = degree(g, mode="out")
  V(g)[!V(g)$indeg]$type = "P"
  V(g)[V(g)$indeg &  V(g)$outdeg]$type = "I"
  V(g)[!V(g)$outdeg]$type = "D"
  # Attach also attributes to the graph
  E(g)$name = paste0(ends(g, E(g))[,1], ends(g, E(g))[,2], sep="_")
  i = ends(g, E(g), names=FALSE)[, 1]
  j = ends(g, E(g), names=FALSE)[, 2]
  E(g)$type = paste0(V(g)$type[i], V(g)$type[j])
  return(g)
}





#' Delete less-important intermediaries of a supply network
#'
#' This function delete intermediaries from a supply network if they are
#' considered as no more important. Deletion can be based on 2 criterias:
#' \describe{
#'   \item{bypassing}{if the density of links between the suppliers and the
#'   customers of an intermediairy node, i.e. its 'supply transitivity', reach a
#'   certain level, it is removed. Allows to favor the emergence of short supply
#'   chains into a rewiring process (default: Inf).}
#'   \item{degree}{if the degree of a node falls below a threshold, it becames
#'   unimportant, and is removed. Allows to favor the emergence of hubs
#'   (default: Inf).}
#' }
#'
#' @details
#' For safety reasons, only 1 intermediary is deleted
#' Indeed, the deletion of 1 can have cascading consequences on the local indices
#' of the other nodes, and conditions would not be respected anymore.
#' @param g an igraph object; the supply network.
#' @param tr numeric; miinimal supply transitivity threshold below which
#' intermediary is deleted (default: Inf).
#' @param deg numeric; degree under which intermediary is deleted (default: Inf).
#' @param cuts boolean; if FALSE (default), the removal of cut-vertices
#' identified with \code{\link[igraph]{articulation_points}} is forbidden.
#'
#' @seealso \code{\link{supplyTransitivity}}
#'
#' @return an igraph object with 1 less intermediary.
#' @export
#'
#' @examples
#' library(igraph)
#' g = make_tree(5)
#' ## Make a transitive closure: all intermediaries are bypassed
#' m = floydAlgo(g)
#' m[m>1] = 1 # To adjacency matrix
#' g = graph_from_adjacency_matrix(m)
#' plot(g)
#' # Vertex 2 is bypassed, so deleted
#' g2 = deleteIntermediaries(g)
#' plot(g2)
#'
deleteIntermediaries = function(g,
                                tr   = Inf,
                                deg  = Inf,
                                cuts = FALSE)
{
  # Get intermediaires if any
  v = V(g)[degree(g, mode='in') & degree(g, mode='out')]
  # v = V(g)[type=="I"]
  if (!length(v)) {
    message("No intermediaries in the graph")
    return(g)
  }
  # Delete intermediaries with a supply transitivity index below the threshold
  if (tr <= 1)
    v = v[supplyTransitivity(g, v) >= tr]
  if (!length(v)) {
    message("No intermediaries can be deleted")
    return(g)
  }
  # Delete intermediaries with a degree under the threshold
  if (is.finite(deg))
    v = v[degree(g, v) < deg]
  if (!length(v)) {
    message("No intermediaries can be deleted")
    return(g)
  }
  # The deletion should not isolate another vertices
  if (!cuts)
    v = v[!v %in% articulation_points(g)]
  if (!length(v)) {
    message("No intermediaries can be deleted without cutting the graph")
    return(g)
  }
  # Delete 1 intermediary from the graph only
  if (length(v)==1) {
    g = delete_vertices(g, v)
  } else{
    g = delete_vertices(g, sample(v, 1))
  }
}







#' Turn a short supply chain into a long supply chain
#'
#' Choose a random short supply-chain edge going directly from producers to
#' distributors, delete it, and replace it by a path of two edges length passing
#' through an intermediary chosen randomly.
#'
#' @param g an igraph object.
#'
#' @return an igraph object.
#' @export
#'
#' @examples
#' ## Turn all short SC into long SC
#' library(igraph)
#' g = generateSupplyNet(E=200)
#' while (length(E(g)[E(g)$type=="PD"]))
#'   g = lengthenShortEdge(g)
#' ecount(g)
#'
lengthenShortEdge = function(g)
{
  g = updateGraphAttributes(g)
  # Get a random P-D edge if any
  e = E(g)[E(g)$type=="PD"]
  if (!length(e))
    return(g)
  e  = sample(e, 1)
  vp = ends(g, e)[1]
  vd = ends(g, e)[2]
  # Get a random intermediary if any, and skip otherwise
  inter = V(g)[V(g)$type=="I"]
  if (!length(inter))
    return(g)
  vi = sample(names(inter), 1)
  # Replace short PD edge by two PI-ID edges
  g = delete_edges(g, e)
  e_pi = c(vp, vi)
  e_id = c(vi, vd)
  g = add_edges(g, c(e_pi, e_id))
  g = updateGraphAttributes(g)
}


