
#' Compute length of shortest path between each pair of points.
#'
#' Based on \code{\link[Rfast]{floyd}} function. If the graph is sparse,
#' \code{floyd} seems to use the Johnson algorithm rather than
#' more complex Floyd-Warhall algorithm. C++ code difficult to understand:
#' https://github.com/cran/Rfast/blob/master/src/floyd_john.cpp. Adpated to
#' Directed Acyclic graph, Floyd-Warshall algorithm is used to apply transitive
#' closures.
#'
#' @param x An igraph object or a matrix. Matrix values represent the edges cost
#' values. If igraph object, it is automatically converted to adjacency matrix.
#'
#' @return A matrix m where he elements denote the length of the shortest path
#' between each pair of points. m[i, j] is zero it means there is not any path
#' from i to j, or a cost of 0.
#' @export
#'
#' @importFrom igraph as_adjacency_matrix
#' @importFrom Rfast floyd
#' @examples
#' library(igraph)
#' set.seed(123)
#' g = make_tree(5)
#' plot(g)
#' ## Make a graph transitive closure with FW algo
#' m = floydAlgo(g)
#' m[m>1] = 1 # To adjacency matrix
#' g_closed = graph_from_adjacency_matrix(m)
#' plot(g_closed)
#'
floydAlgo = function(x)
{
  if (!is(x, c("igraph", "matrix")))
    stop("x require an igraph object or an adjacency matrix")
  if (is(x, "igraph")) {  # if x is a graph, get its adjacency matrix
    x = as.matrix(as_adjacency_matrix(x))
    if (length(x[x>1])) {
      x[x>1] = 1
      message("Eliminate graph double edges to compute floyd algo")
    }
    diag(x) = 0 # Eliminate loops if any
  }
  # Compute Floyd-Warshall (-Johnson) algorithm
  x[x==0] = Inf
  mfloyd = Rfast::floyd(x)
  # Reset adjacency matrix attributes
  mfloyd[mfloyd==Inf] = 0
  diag(mfloyd) = 0
  rownames(mfloyd) = rownames(x)
  colnames(mfloyd) = colnames(x)
  return(mfloyd)
}
