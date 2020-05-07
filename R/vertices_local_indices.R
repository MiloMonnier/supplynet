
#' Summarize degree sequence of a vertex neighbors
#'
#' For a given vertex, get its neighbors degree sequence, and summarize it with
#' a function. Can be used to identify a kind of articulations points: if the
#' minimal degree of a vertex neighbors is > 1, its deletion does not isolate
#' another one.
#'
#' @param g igraph object.
#' @param vs igraph.vs; vertices sequence of which the local index will be
#'  calculated (default: all vertices).
#' @param ngh.mode character; the neighbors mode, either 'all', 'in', 'out'
#' (default: 'all').
#' @param deg.mode character; the degree mode, either 'all', 'in', 'out'
#' (default: 'all').
#' @param FUN function used to summarize the degree sequence (default: min).
#'
#' @return A numeric vector of the same length than vertices sequence vs.
#' @export
#'
#' @importFrom igraph neighbors
#' @importFrom igraph degree
#' @examples
#' library(igraph)
#' set.seed(123)
#' g = erdos.renyi.game(10, 20, 'gnm', directed=TRUE)
#' plot(g)
#' neighborsDegree(g, FUN=max)
#'
neighborsDegree = function(g,
                           vs       = V(g),
                           ngh.mode = c("all", "in", "out"),
                           deg.mode = c("all", "in", "out"),
                           FUN      = min)
{
  if (!is(vs,"igraph.vs"))
    stop("vs require an igraph.vs class")
  ngh.mode = match.arg(ngh.mode)
  deg.mode = match.arg(deg.mode)
  if (!is(FUN,"function"))
    stop("FUN require a function")
  # For each vertex, 1. get its neighbors; 2. their degree; 3. summarize
  myFun = function(v) {
    ngh     = neighbors(g, v, mode=ngh.mode)
    ngh_deg = degree(g, ngh, mode=deg.mode)
    FUN(ngh_deg)
  }
  sapply(vs, myFun)
}





# Used to deleted intermediary vertices "bypassed" by an high connexion density
# between their suppliers and customers

#' Local supply transitivity of intermediaires
#'
#' Customized version of local transitivity, adapted to intermediaries of a
#' supply network. Original transitivity measures the probability that the
#' adjacent vertices of a vertex are connected. This is sometimes also called
#' the clustering coefficient. This function measures the probability that the
#' in-neighbors (suppliers) and the out-neighbors (customers) of a vertex (an
#' intermediary) are connected. It reveals how an intermediary can be
#' "by-passed", "short-circuited" by parallel links, and thus useless.
#'
#' @param g igraph object.
#' @param vs igraph.vs; vertices sequence of which the local index will be
#' calculated (default: all vertices).
#'
#' @seealso \code{\link[igraph]{transitivity}}
#'
#' @return numeric vector of the same length than vertices sequence vs.
#' @export
#'
#' @importFrom igraph as_adjacency_matrix
#' @importFrom igraph neighbors
#' @examples
#' ##  Supply Transitivity of in termediary reaches 1 after a transitive closure
#' library(igraph)
#' set.seed(123)
#' g = make_tree(5)
#' lay = layout_nicely(g)
#' plot(g, layout=lay)
#' supplyTransitivity(g, V(g)[2])
#' # Make transitive closure
#' m = floydAlgo(g)
#' m[m>1] = 1 # To adjacency matrix
#' g = graph_from_adjacency_matrix(m)
#' plot(g, layout=lay)
#' supplyTransitivity(g, V(g)[2])
#'
supplyTransitivity = function(g,
                              vs = V(g))
{
  if (!is(vs,"igraph.vs"))
    stop("vs require an igraph.vs class")
  m = as.matrix(as_adjacency_matrix(g))
  myFun = function(v) {
    supp = neighbors(g, v, mode="in")
    cust = neighbors(g, v, mode="out")
    mean(m[supp, cust, drop=FALSE])
  }
  res = sapply(vs, myFun)
  res[is.nan(res)] = 0
  res
}

#' Vertex relative degree
#'
#' Relative in- or out-degree of a vertex measures the balance between its
#' number of incoming and outgoing edges.
#'
#' @param g igraph object.
#' @param vs igraph.vs; vertices sequence of which the local index will be
#' calculated (default: all vertices).
#' @param mode either 'in' our 'out': the relative degree mode (default: 'in').
#'
#' @return A numeric vector of the same length than vertices sequence vs.
#' @export
#'
#' @examples
#' library(igraph)
#' set.seed(123)
#' g = erdos.renyi.game(10, 20, 'gnm', directed=TRUE)
#' relativeDegree(g)
#'
relativeDegree = function(g,
                          vs   = V(g),
                          mode = c("in", "out"))
{
  mode = match.arg(mode)
  res = degree(g, vs, mode=mode) / degree(g, vs)
  res[is.nan(res)] = 0
  res
}

# USefull for further applications
# Make transitive matrix undirected
#  m_path = m_path + t(m_path)
# Producers are now reachable from distributors for example
