
#' Supply network reachability
#'
#' Reachability of a supply network measures the probability that a source
#' node (a producer) is connected to a target node through a path
#' It is equal to the number of supply paths from the sources set S to sink
#' set T, divided by the overall number of possible paths (|S| * |T|).
#'
#' @param g igraph object.
#'
#' @return A numeric between 0 and 1.
#' @export
#'
#' @examples
#' ## In a tree, reachability is equal to 1
#' library(igraph)
#' g = make_tree(10)
#' reachability(g)
#' ## Theoretical 1 hub-centralized supply network
#' #TODO
#'
reachability = function(g)
{
  # TODO Comprendre pourquoi certains scénarioos ne sont pas des DAG
  # if (!is_dag(g))
  #   stop("g require a Directed Acyclic Graph (DAG)")
  # Get shortest path length matrix with sources in rows and targets in cols
  m = floydAlgo(g)
  m = m[rowSums(m) & !colSums(m),  !rowSums(m) & colSums(m), drop=FALSE]
  # Nb of paths / Nb of possible paths from sources to targets
  length(m[m>0]) / (nrow(m) * ncol(m))
}

#' Supply network reachability-efficiency
#'
#' Divide the reachability of the network by the effort spent by producers and
#' distributors, i.e. their average degree.
#'
#' @param g igraph object.
#'
#' @return A numeric between 0 and 1.
#' @export
#'
#' @examples
#' library(igraph)
#' g = make_tree(10)
#' efficiencyRe(g)
#'
efficiencyRe = function(g)
{
  # Compute average degree of sources and targets and distributors
  m = as.matrix(g[]) # adjacency matrix
  S_deg = rowSums(m[!colSums(m), , drop=FALSE])
  T_deg = colSums(m[, !rowSums(m), drop=FALSE])
  ST_avg_deg = mean(c(S_deg, T_deg))
  # Divide reachability by avg degree
  reachability(g) / ST_avg_deg
}

# TODO Check
# https://github.com/cwatson/brainGraph/blob/master/R/graph_efficiency.R

# TODO Rather than PD average degree, we can use the median degree ?

#' Latora and Marchiori efficiency
#'
#' Adapted
#' Based on characterisic âth length Intergates spatial distance.
#' Normalized by ideal efficiency, a situation in whihc all nodes are directly
#' connected
#'  Unlike average distance, this metric is correctly defined even if the graph
#' is not connected.
#'
#' @details For aggregated graphs, road distance cannot be used. Thus, its uses
#' euclidian distance based on \code{x} and \code{y} vertices attributes.
#'
#' @references
#' Latora, V., & Marchiori, M. (2001). Efficient Behavior of Small-World Networks.
#' Physical Review Letters, 87(19), 198701.
#' https://doi.org/10.1103/PhysRevLett.87.198701
#'
#' @param g An igraph object.
#'
#' @return A numeric between 0 and 1.
#' @export
#'
#' @importFrom fields rdist
#' @importFrom Rfast floyd
#' @examples
#' library(igraph)
#' set.seed(123)
#' ## Generate 100 random spatial networks with increasing density
#' genGraph = function(p) {
#'   g = erdos.renyi.game(10, p)
#'   V(g)$x = runif(10)
#'   V(g)$y = runif(10)
#'   g
#' }
#' x = seq(0, 1, by=0.01)
#' lg = lapply(x, genGraph)
#' ## Efficiency increases with density
#' y = sapply(lg, efficiencyLM)
#' plot(x, y)
#' ## Local efficiency is average efficiency of local subgraphs
#' ## dg = decompose.graph(g)
#' ## mean(sapply(dg, efficiencyLM))
efficiencyLM = function(g)
{
  g = as.undirected(g)
  # A - Adjacency matrix
  A = g[]
  # S - Spatial (euclidian) distance matrix
  coords = cbind(V(g)$x, V(g)$y)
  S = fields::rdist(coords)
  S[S==0] = 0.000001  # Same locations (MIN wholesalers) cause problems
  diag(S) = 0
  # Ls - Sum of spatial distances throughout all possible paths
  Af = A
  Af[Af==0] = Inf # For Floyd algo, no path = Infinite distance
  Ls = Rfast::floyd(Af * S)
  # L - Inverse path length matrix
  Ds = 1 / Ls
  diag(Ds) = 0 # Replace Inf
  # Compute efficiency E
  N = length(V(g))
  E = sum(Ds)/(N*(N-1))
  # Normalize E by ideal E in which all pairs are connected
  diag(S) = Inf
  Dsi = 1 / S # Ideal spatial distance through paths
  Ei = sum(Dsi)/(N*(N-1))
  E / Ei
}
# Customizations fails:
# Eliminate target from rows and sources from columns
# L = L[rowSums(A), colSums(A)]
# Keep only sources in rows and targets in columns
# L = L[rowSums(A) & !colSums(A),  !rowSums(A) & colSums(A)]
# E = sum(L) / (nrow(L) * ncol(L))
# which(Eid==Inf, arr.ind = T) # Identify elements position in a matrix





#' Aeverage Delivery Efficiency
#'
#' This efficiency metric combine both the number of producer nodes that can be
#' accessed by a distributor node and also the (topological) distance at which
#' each supply nodes is located.
#'
#' @details
#' First, inverse supply path length between all producer-distributor
#' pair is computed.
#' distributors nodes AVG_DEF
#' AVG_DEF is equal to the mean of individual
#'
#' Based on inverse supply pat
#' Decreasing function set how the weight of the supplier decreases with its rank
#'
#' @references
#' Zhao, K., Kumar, A., & Yen, J. (2011). Achieving High Robustness in Supply
#' Distribution Networks by Rewiring. IEEE Transactions on Engineering Management,
#' 58(2), 347–362.
#'
#' @param g An igraph object; the supply network.
#' @param decr.FUN function; how the weight of a supplier in the metric depend
#' decrease with its rank according to its topological distance from target node.
#'
#' @return A positive numeric.
#' @export
#'
#' @importFrom methods is
#' @importFrom igraph as_adj
#' @importFrom Matrix rowSums
#' @importFrom Matrix colSums
#' @importFrom Rfast floyd
#'
#' @examples
#' library(igraph)
#' g = generateSupplyNet()
#' N = seq(100)
#' ## For a tree with 1 producer, AVG_DEF decreases with number of nodes
#' lg = lapply(N, make_tree)
#' eff = sapply(lg, AVG_DEF)
#' plot(N, eff)
#'
AVG_DEF = function(g,
                   decr.FUN = function(x) 1/x)
{
  if (!is(decr.FUN, "function"))
    stop("decr.FUN require a function")
  # A is adjacency matrix
  A = as_adj(g)
  Af = A
  Af[Af==0] = Inf
  # L is path length matrix
  L = Rfast::floyd(Af)
  L[L==Inf] = NA
  diag(L) = NA
  # Keep only sources in rows and targets in columns
  L = L[rowSums(A) & !colSums(A),  !rowSums(A) & colSums(A), drop=FALSE]
  # R is rank matrix: for each demand node, rank its suppliers by distance
  # Nearest supplier has rank 1, etc
  R = apply(L, 2, rank, na.last="keep", ties.method="first")
  # R = apply(L, 2, rank, na.last="keep", ties.method="min")
  mean(colSums(1/L ^ (1/decr.FUN(R)), na.rm=TRUE))
}




