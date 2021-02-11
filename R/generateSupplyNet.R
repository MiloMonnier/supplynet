#' Generate theoretical supply network
#'
#' Generate a theorectical supply network composed of three types of vertices:
#' producers, intermediaries, and distributors. Output graph is a Directed
#' Acyclic Graph (DAG) in which every producers and intermediaries has >= 1
#' customer, and every intermediaries and distributors have a supplier.
#'
#' @param P numeric; number of producers (default: 67).
#' @param I numeric; number of intermediaries (default: 18).
#' @param D numeric; number of distributors (default: 70).
#' @param E numeric; number of edges (default: 228).
#'
#' @return igraph object; a supply network.
#' @export
#'
#' @importFrom igraph which_loop
#' @importFrom igraph which_multiple
#'
#' @examples
#' net = generateSupplyNet()
#' plot(net)
#'
generateSupplyNet = function(P=67, I=18, D=70, E=228)
{
  # Vertices table
  N = P+I+D
  type = c(rep("P", P), rep("I", I), rep("D", D))
  name = seq(N)
  x = runif(N)
  y = runif(N)
  v = data.frame(name=name, type=type, x=x, y=y)
  # Edges table
  vP = name[type=="P"]
  vI = name[type=="I"]
  vD = name[type=="D"]
  formals(sample)$replace = TRUE
  from = sample(c(vP, vI), E)
  to   = sample(c(vI, vD), E)
  e = cbind(from, to)
  # Build graph
  g = graph_from_data_frame(e, directed=TRUE, v)
  # Clean is from isolated vertices and loops
  g = g - V(g)[!degree(g)]
  g = delete_edges(g, E(g)[which_loop(g)])
  g = delete_edges(g, E(g)[which_multiple(g)])
  g = updateGraphAttributes(g)
}

