#' Add random edges to a graph
#'
#' @param g An igraph object.
#' @param n Number of edges to add.
#' @param from igraph.vs or character names; vertices sequence of possible edges
#' origins (default: NULL).
#' @param to igraph.vs or character names; vertices sequence of possible edges
#' origins (default: NULL).
#' @param v2connect igraph.vs or character names; vertices sequence edges origin
#' or destinations must be attached to (default: NULL).
#' @param multi.edges boolean; wheter or not multiple edges can be created
#' (default: FALSE).
#' @param iie.ceil numeric; maximal number of edges between intermediaries
#' in the graph (default: 4). TODO Must be changed to Inf
#'
#' @return An igraph object with \code{n} more edges.
#' @export
#'
#' @importFrom reshape2 melt
#' @examples
#' ## Generate a theoretical supply network, define producers (P),
#' ## intermediaries (I) and distributors (D) vertices.
#' library(igraph)
#' g = make_tree(10)
#' V(g)[!degree(g, mode="in")]$type = "P"
#' V(g)[degree(g, mode="in") & degree(g, mode="in")]$type = "I"
#' V(g)[!degree(g, mode="out")]$type = "D"
#' V(g)$color = c("red","green","blue")[factor(V(g)$type)]
#' plot(g)
#' ## Add 5 edges going from P to D (short supply chains)
#' g2 = addRandomEdges(g, n=5, from=V(g)[type=="P"], to=V(g)[type=="D"])
#' plot(g2)
#' ## Add 3 edges connected to I (belonging long supply chains)
#' g2 = addRandomEdges(g, n=3, v2connect=V(g)[type=="I"])
#'
addRandomEdges = function(g,
                          n           = 1,
                          from        = NULL,
                          to          = NULL,
                          v2connect   = NULL,
                          multi.edges = FALSE,
                          iie.ceil    = 4)
{
  if (is(from,"igraph.vs"))
    from = names(from)
  if (is(to,"igraph.vs"))
    to = names(to)
  if (is(v2connect,"igraph.vs"))
    v2connect = names(v2connect)
  # Get all possible unconnected vertices pairs
  madj = as.matrix(as_adjacency_matrix(g))
  df = reshape2::melt(madj)
  colnames(df) = c("from","to","connected")
  df$from = as.character(df$from)
  df$to   = as.character(df$to)
  v_prod  = V(g)[V(g)$type=="P"]$name
  v_inter = V(g)[V(g)$type=="I"]$name
  v_distr = V(g)[V(g)$type=="D"]$name
  df = df[!df$to   %in% v_prod, ]
  df = df[!df$from %in% v_distr, ]
  df = df[df$from  !=   df$to, ]
  # Restrictions to vertices set if any
  if (length(from))
    df = df[df$from %in% from, ]
  if (length(to))
    df = df[df$to %in% to, ]
  if (length(v2connect))
    df = df[(df$from %in% v2connect | df$to %in% v2connect), ]
  # Double edges can be avoided
  if (!multi.edges)
    df = df[!df$connected, ]
  # Do not create too much edges between intermediaries if limited
  i = ends(g, E(g), names=FALSE)[, 1]
  j = ends(g, E(g), names=FALSE)[, 2]
  E(g)$type = paste0(V(g)[i]$type, V(g)[j]$type)
  if (length(E(g)[E(g)$type=="II"]) >= iie.ceil)
    df = df[!(df$from %in% v_inter  &  df$to %in% v_inter), ]
  if (!nrow(df)) {
    message("addRandomEdges() - Cancel: Impossible to fill graph with more edges")
    return(g)
  }
  if (nrow(df) < n)
    n = nrow(df)
  # Sample vertices pairs and create new edge with its attributes
  m = df[sample(nrow(df), n), c("from","to")]
  new_e = as.vector(t(m))
  g = add_edges(g, new_e)
}

# name = paste(m$from, m$to, sep="_")
# type = paste0(V(g)[m$from]$type, V(g)[m$to]$type)
# attr = list(name=name, type=type)
# g = add_edges(g, new_e, attr=attr)
