
#' Swap two edges of a igraph object.
#'
#' Extends \code{\link[igraph]{rewire}} used with \code{\link[igraph]{keeping_degseq}}
#' with tunable parameters. Possible to force the swap of two edges of different
#' types, or of the same type. Edges swapping can be used to realize a Degree
#' Preserving Randomization (DPR). One of the two edges can be imposed (e1).
#'
#' @param g An igraph object
#' @param es igraph.es; edge sequence among which to choose edges to swap.
#' (default: all edges).
#' @param e1 igraph.es; one of the two swapped edges can be imposed (default: NULL).
#' @param edges.types character string; either 'random', 'same' or 'different':
#' of what type must be edges swapped. Requir E(g)$type attribute (default: 'random').
#' @param multi.edges boolean; whether or not multiple edges creation is allowed
#' (default: FALSE).
#'
#' @return An igraph object.
#' @export
#'
#' @examples
#' ## Generate a theoretical supply network, define producers (P), intermediaries (I)
#' ## and distributors (D) vertices.
#' library(igraph)
#' g = make_tree(10)
#' V(g)[!degree(g, mode="in")]$type = "P"
#' V(g)[degree(g, mode="in") & degree(g, mode="out")]$type = "I"
#' V(g)[!degree(g, mode="out")]$type = "D"
#' V(g)$color = c("green","red","yellow")[factor(V(g)$type, levels=c("P","I","D"))]
#' plot(g)
#' ## Swap edges to conserve supply from P to D.
#' for (i in 1:100) {
#'   i = ends(g, E(g), names=FALSE)[,1]
#'   j = ends(g, E(g), names=FALSE)[,2]
#'   E(g)$type = paste0(V(g)[i]$type, V(g)[j]$type)
#'   g = swapEdges(g, edges.types="diff")
#'   if (length(E(g)[which_multiple(g, E(g))]))
#'      stop("Multiedges")
#'   if (length(E(g)[which_loop(g, E(g))]))
#'      stop("Loops")
#'   if (length(V(g)[degree(g)==0]))
#'      stop("Isolated vertex")
#' }
#' plot(g)
#'
swapEdges = function(g,
                     es          = E(g),
                     e1          = NULL,
                     edges.types = c("random","same","different"),
                     multi.edges = FALSE)
{
  if (!is(es, "igraph.es"))
    stop("es require a igraph.es class object")
  if (!is.null(e1) && (!is(e1, "igraph.es") || length(e1) != 1))
    stop("e1 require a igraph.es class object of length 1")
  edges.types = match.arg(edges.types)
  if (edges.types %in% c("same","different")) {
    if (is.null(E(g)$type))
      stop("Require an E(g)$type attribute")
    if (any(is.na(E(g)$type)))
      stop("NA detected in E(g)$type")
  }

  # Choose first edge e1 if not already passed
  if (is.null(e1))
    e1 = sample(es, 1)
  # Choose second edge e2 among edge sequence "es"
  e1ij = ends(g, e1)
  es = es[!ends(g,es)[,1] %in% e1ij] # Not adjacent to e1
  es = es[!ends(g,es)[,2] %in% e1ij]
  # Of same or different type
  if (edges.types=="same" && any(es$type==e1$type)) {
    es = es[es$type==e1$type]
  } else if (edges.types=="different" && any(es$type!=e1$type)) {
    es = es[es$type!=e1$type]
  }
  # To avoid creating multi-edges, e2 vertices must not be neighbors of e1
  if (!multi.edges) {
    ngh_e1i = neighbors(g, e1ij[1], mode="all")
    ngh_e1j = neighbors(g, e1ij[2], mode="all")
    ngh_e1  = names(c(ngh_e1i, ngh_e1j))
    es = es[!ends(g,es)[,1] %in% ngh_e1]
    es = es[!ends(g,es)[,2] %in% ngh_e1]
  }
  # Choose random edge e2 into the remaining edge set
  if (!length(es)) {
    message(paste("Swap of edges of", edges.types, "types failed: returned g"))
    return(g)
  }
  e2 = sample(es, 1)
  e2ij = ends(g, e2)
  # Swap e1 & e2: delete it and replace by e3 & e4
  g = delete_edges(g, c(e1, e2))
  e3 = c(e1ij[1], e2ij[2])
  e4 = c(e2ij[1], e1ij[2])
  g = add_edges(g, c(e3, e4))
}
# TODO Add inter-inter edges ceil ?
