

#' Rewire origin or destination of an edge
#'
#' Preferential attachment can be included by passing some paramters to ...
#'
#' @param g An igraph object
#' @param edge An igraph.es edge to rewire. Default is 1 random edge.
#' @param what For directed graphs, what to rewire: "origin" or "destination" ?
#' Default is "origin".
#' @param vs igraph.vs vertices towards which edge origin or destination can be rewired.
#' @param multi.edges Wheter or not multi edges can be created during the process.
#' Default is FALSE.
#' @param ... Other parameters passed to \code{\link{getVertices}} function.
#'
#' @return An igraph object with 1 edge rewired.
#' @export
#'
#' @examples
#' library(igraph)
#' g = generateSupplyNet()
#' for (i in 1:100)
#'   g = rewireEdge(g)
#'
rewireEdge = function(g,
                      edge        = sample(E(g), 1),
                      what        = c("origin","destination"),
                      vs          = V(g),
                      multi.edges = FALSE,
                      ...)
{
  if (is.null(V(g)$name))
    V(g)$name  = 1:length(V(g))
  if (!is(edge,"igraph.es") || length(edge) != 1)
    stop("edge require an igraph.es class object of length 1")
  if (!is.null(vs) && !is(vs,"igraph.vs"))
    stop("vs require a igraph.vs class object")
  what = sample(what, 1)
  # Get extremities of the edge to rewire
  vi = ends(g, edge, names=TRUE)[, 1][1]
  vj = ends(g, edge, names=TRUE)[, 2][1]
  # Filter potential new origin or destinations
  if (what=="origin") {
    vs = vs[vs$type %in% c("P","I")]
  } else if (what=="destination") {
    vs = vs[vs$type %in% c("I","D")]
  }
  # Avoid loops and double edges if wanted
  vs = vs[!vs$name %in% c(vi, vj)]
  if (!multi.edges) {
    if (what=="origin") {
      vs = vs[!vs %in% neighbors(g, vj, mode="in")]
    } else if (what=="destination") {
      vs = vs[!vs %in% neighbors(g, vi, mode="out")]
    }
  }
  # Choose vertex to rewire the edge to
  if (!length(vs)) {
    message("No vertices towards which to rewire")
    return(g)
  }
  vk = getVertices(g, n=1, vs=vs, ...)$name
  # Generate new edge
  if (what=="origin") {
    new_e = c(vk, vj)
  } else if (what=="destination") {
    new_e = c(vi, vk)
  }
  # Replace the old by the new edge
  g = delete_edges(g, edge) %>%
    add_edges(new_e) %>%
    updateGraphAttributes()
}

# Avoid to create forbidden edges: double edges or excessive II edges
# if (!is.null(iie.ceil)  &&  e_type=="II"  &&  length(V(g)[type=="II"]) >= iie.ceil) {
#   if (verbose)
#     message(paste("rewireEdge() - Cancel: number of Inter-->Inter edges ceiled to", iie.ceil))
#   return(g)
# }


#' Swap two edges of a supply network
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
    message(paste("Impossible to swap of edges of", edges.types, "types"))
    return(g)
  }
  e2 = sample(es, 1)
  e2ij = ends(g, e2)
  # Swap e1 & e2: delete it and replace by e3 & e4
  g = delete_edges(g, c(e1, e2))
  e3 = c(e1ij[1], e2ij[2])
  e4 = c(e2ij[1], e1ij[2])
  g = add_edges(g, c(e3, e4))
  g = updateGraphAttributes(g)
}
# TODO Add inter-inter edges ceil ?


#' Add random edges to a supply network
#'
#' @param g An igraph object.
#' @param n numeric; the number of edges to add.
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
    message("Impossible to add edges anymore")
    return(g)
  }
  if (nrow(df) < n)
    n = nrow(df)
  # Sample vertices pairs and create new edge with its attributes
  m = df[sample(nrow(df), n), c("from","to")]
  new_e = as.vector(t(m))
  g = add_edges(g, new_e)
  g = updateGraphAttributes(g)
}



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
    message("Impossible to delete anymore edges")
    return(g)
  }
  # Don't delete bridges edges
  if (!bridges) {
    bridges = getBridges(g)
    es = es[!es$name %in% bridges$name]
  }
  if (!length(es)) {
    message("Impossible to delete anymore edges")
    return(g)
  }
  # Choose an edge among the edge set left and delete it
  g = delete_edges(g, sample(es, n))
}


#' Adjust the average degree of a supply network
#'
#' During a rewiring process, several operations such as nodes deletion
#' can make change the average degree of a graph. This function aims to
#' re-adjust the number of edges to a given level by adding or deleting edges
#' randomly.
#'
#' @param g an igraph object.
#' @param avg.degree numeric; the expected average degree.
#' @param ... other arguments passed to \code{\link{addRandomEdges}} to
#' constrain the add of edges. For example, if we want the edges to be added
#' to long supply chains, we pass \code{v2connect=V(g)[type=="I"]}
#'
#' @return an igraph object.
#' @export
#'
#' @examples
#' library(igraph)
#' g = generateSupplyNet()
#' plot(g)
#' ecount(g) / vcount(g)
#' g = adjustAvgDegree(g, avg.degree=3)
#' ecount(g) / vcount(g)
#' plot(g)
#'
adjustAvgDegree = function(g,
                           avg.degree = 1.470968,
                           ...)
{
  expE = round(vcount(g) * avg.degree)
  while (length(E(g)) < expE)
    g = addRandomEdges(g, ...)
  while (length(E(g)) > expE)
    g = deleteRandomEdges(g, supply.cuts=FALSE, bridges=FALSE)
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
#'   \item{degree}{if the degree of a node falls and is equal or lower to this
#'   threshold, it is removed. Allows to favor the emergence of hubs
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
    v = v[supplyTransitivity(g, v) > tr]
  if (!length(v)) {
    message("No intermediaries can be deleted")
    return(g)
  }
  # Delete intermediaries with a degree under the threshold
  if (is.finite(deg))
    v = v[degree(g, v) <= deg]
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
# load("~/Dropbox/food_circuits/data/3-network_spatial_cleaned.RData")
# g = deleteIntermediaries(g, tr=0.01)
# vcount(g)


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
  if (!length(e)) {
    message("No more short supply chains to lengthen")
    return(g)
  }
  e  = sample(e, 1)
  vp = ends(g, e)[1]
  vd = ends(g, e)[2]
  # Get a random intermediary if any, and skip otherwise
  inter = V(g)[V(g)$type=="I"]
  if (!length(inter)) {
    message("No more intermediaries nodes")
    return(g)
  }
  vi = sample(names(inter), 1)
  # Replace short PD edge by two PI-ID edges
  g = delete_edges(g, e)
  e_pi = c(vp, vi)
  e_id = c(vi, vd)
  g = add_edges(g, c(e_pi, e_id))
  g = updateGraphAttributes(g)
}



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




