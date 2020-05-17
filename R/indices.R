
# VERTICES LOCAL INDICES --------------------------------------------------

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


#' Vertex supply transitivity (for intermediaires)
#'
#' Customized version of local transitivity index, adapted to intermediaries of a
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
# For further applications: Make transitive matrix undirected
#  m_path = m_path + t(m_path)
# Producers are now reachable from distributors for example



# DEGREE DISTRIBUTION ---------------------------------------------

#' Normalized entropy of a distribution
#'
#' Shannon entropy estimate the average minimum number of bits needed to encode
#' a string of symbols, based on the frequency of the symbols. A perfectly
#' homogeneous distribution will have a normalized entropy of 1. In case of networks,
#' maximizing the entropy of the degree distribution is equivalent to smooth the
#' degree inequalities
#'
#'  @seealso Based on \code{\link[DescTools]{Entropy}}.
#' https://github.com/cran/DescTools/blob/master/R/StatsAndCIs.r#L4776
#'
#' @param x numeric vector; the statistical distribution
#' @param N numeric; expected number of elements of x. Allows to compare
#' entropies of distribution with different number of elements, if an element
#' is deleted from one of the 2, such as in a network rewiring model, entropy
#' remain comparable.
#'
#' @return a numeric between 0 an 1.
#' @export
#'
#' @examples
#' library(igraph)
#' library(DescTools)
#' ## Compare entropy of different distributions
#' x = rep(1, 5)         # Perfectly homogeneous = maximal H
#' y = seq(1, 5)         # Linear ascending
#' z = cumprod(c(1,rep(2,4))) # Scale ascending = minimal H
#' m = cbind(x,y,z)
#' # matplot(m, type="b")
#' ## Raw entropy vs normalized entropy
#' apply(m, 2, Entropy)
#' apply(m, 2, normEntropy)
#' ## Generate scale free networks with different powerlaws
#' powers = seq(1, 2, by=0.2)
#' nets = lapply(powers, function(x) barabasi.game(n=100, power=x))
#' ## High power law exponent gives low normalized entropy
#' sapply(nets, function(x) fit_power_law(degree(x))$alpha)
#' sapply(nets, function(x) normEntropy(degree(x)))
#' ## Same powerlaws but different number of vertices
#' n_vs = seq(1000, 10000, by=1000)
#' nets = lapply(n_vs, function(x) barabasi.game(n=x, power=1.3))
#' sapply(nets, function(x) fit_power_law(degree(x))$alpha)
#' sapply(nets, function(x) normEntropy(degree(x)))
#' #TODO Prove utility of increased v
#'
normEntropy = function(x, N=0)
{
  if (sum(x)==0)
    return(0)
  if (N > length(x)) { # Extend x with "trailing zeros"
    x = c(x, rep(0, N-length(x)))
  } else {
    N = length(x)
  }
  P = x / sum(x)
  H = -sum(ifelse(P>0, P * log(P, base=2), 0))
  H / log(N, base=2)
}


#' Density of a Directed Acyclic Graph (DAG)
#'
#' A Directed Acyclic Graph (DAG) is a hierarchical structure of 3 levels of
#' vertices: the source set, the intermediaries set and the targets set. Given
#' that edges outgoing from targets and incoming to sources cannot exist, this
#' metric compute a relevant graph density for DAGs?
#'
#' @param g an igraph object; a directed acycic graph, with is_dag=TRUE.
#'
#' @return a numeric between 0 and 1.
#' @export
#'
#' @importFrom igraph is_dag
#' @examples
#' library(igraph)
#' g = make_tree(10)
#' edge_density(g)
#' edge_density(as.undirected(g))
#' is_dag(g)
#' dagDensity(g)
#'
dagDensity = function(g)
{
  if (!is_dag(g))
    stop("g require a Directed Acyclic Graph (DAG)")
  # Disriminate auto 3 vertices levels: sources, intermediaries, targets
  P = length(V(g)[!degree(g, mode="in")])
  I = length(V(g)[degree(g, mode="in") & degree(g, mode="out")])
  D = length(V(g)[!degree(g, mode="out")])
  # Comute density
  maxE = P*I + P*D + I*D + I*(I-1)
  length(E(g)) / maxE
}

# SUPPLY CHAINS -----------------------------------------------------------

#' Overall number of supply chains in a supply network
#'
#' A supply chain is a path starting from a producer and ending to a distributor.
#' A supply network counts as many supply chains as connected producer-distributors
#' pairs.
#'
#' @param g igraph object; the supply network.
#'
#' @return a positive integer.
#' @export
#'
#' @examples
#' library(igraph)
#' g = generateSupplyNet()
#' nbSupplyChains(g)
#'
nbSupplyChains = function(g)
{
  # Compute shortest paths length matrix between producers and distributors
  m = floydAlgo(g)
  vP = V(g)[V(g)$type=="P"]$name
  vD = V(g)[V(g)$type=="D"]$name
  m = m[vP, vD, drop=FALSE]
  length(m[m >= 1])
}


#' Short supply chains edges rate
#'
#' Short supply chains are edges linking directly producers to distributors.
#' This function compute the rate of short SCs edges in the overall edges number.
#'
#' @param g an igraph object; the supply network.
#'
#' @return a numeric between 0 and 1.
#' @export
#'
#' @examples
#' library(igraph)
#' g = generateSupplyNet()
#' shortEdgesRate(g)
#'
shortEdgesRate = function(g)
{
  nShortE = length(E(g)[E(g)$type=="PD"])
  nShortE / length(E(g))
}


#' Average Supply-Path Length (SPL)
#'
#' A Supply-Path is a synonym of Supply Chain. This index is equal to the average
#' of the shortest path length from producers to distributors.
#'
#' @param g an igraph object; the supply network.
#'
#' @return a positive numeric.
#' @export
#'
#' @examples
#' library(igraph)
#' g = generateSupplyNet()
#' avgSPL(g)
#'
avgSPL = function(g)
{
  m = floydAlgo(g)
  vP = V(g)[V(g)$type=="P"]$name
  vD = V(g)[V(g)$type=="D"]$name
  m = m[vP, vD, drop=FALSE]
  mean(m[m>=1])
}


#' Average inverse supply path length
#'
#' First, all shortest path lengths between producers and distributors are
#' computed. Then, their inverse are computed. This index returns the mean of
#' the inverses paths length (and not the inverse of the avgSPL).
#'
#' @param g an igraph object; the supply network.
#'
#' @return a numeric between 0 and 1. 1 corresponds to all short-circuits network.
#' @export
#'
#' @examples
#' library(igraph)
#' g = generateSupplyNet()
#' avgInvSPL(g)
#'
avgInvSPL = function(g)
{
  m = floydAlgo(g)
  vP = V(g)[V(g)$type=="P"]$name
  vD = V(g)[V(g)$type=="D"]$name
  m = m[vP, vD, drop=FALSE]
  m = 1/m
  mean(m * is.finite(m), na.rm = TRUE)
}
