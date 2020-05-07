
#' Apply cascading failures to a supply network
#'
#' The deletion of a vertex or an edge in a supply network can cause other
#' disruptions (cascading failures). This function clean the graph from
#' producers and intermeidaries without customers, and intermediaries
#' or distributors without suppliers. Process is embedded in a while loop
#' to clean all the graph.
#'
#' @param g igraph; a supply network where every node has a 'type' attribute,
#' which can be "P" (producer), "I" (intermediary), or "D" (distributor).
#'
#' @return igraph; a supply network in which every producer has a path towards a
#' distributor, and
#' and every distributor have a producer upstream.
#' @export
#'
#' @importFrom igraph delete_vertices
#' @examples
#' ## Generate a theoretical supply network, define producers (P), inter. (I)
#' ## and distributors (D) vertices.
#' library(igraph)
#' g = make_tree(10)
#' V(g)[!degree(g, mode="in")]$type = "P"
#' V(g)[degree(g, mode="in") & degree(g, mode="in")]$type = "I"
#' V(g)[!degree(g, mode="out")]$type = "D"
#' V(g)$color = c("green","red","yellow")[factor(V(g)$type, levels=c("P","I","D"))]
#' plot(g)
#' ## Delete vertex with highest betweenness
#' g = delete_vertices(g, V(g)[which.max(betweenness(g))])
#' plot(g)
#' ## Some vertices are not supplied by any producer: delete them
#' g = applyCascadingFailures(g)
#' plot(g)
#'
applyCascadingFailures = function(g)
{
  getFailedVertices = function(g) {
    res = list(P_isol    = V(g)[V(g)$type=="P" & degree(g, mode="out")==0],
               I_inzero  = V(g)[V(g)$type=="I" & degree(g, mode="in")==0],
               I_outzero = V(g)[V(g)$type=="I" & degree(g, mode="out")==0],
               D_isol    = V(g)[V(g)$type=="D" & degree(g, mode="in")==0])
    do.call(c, res)
  }
  # Eliminate failed vertices until the last one
  v_failed = getFailedVertices(g)
  while (length(v_failed)) {
    g = delete_vertices(g, v_failed)
    v_failed = getFailedVertices(g)
  }
  return(g)
}


#' Supply Availability Rate
#'
#' Supply Avalability Rate (SAR) compare the number of distributors still supplied
#' in a network o the number of distributors in a reference network. Intended
#' to measure the impacts of successive diisruptions on a supply network. Used
#' in robustness assessment process.
#'
#' @param g0 igraph; the initial supply network.
#' @inheritParams applyCascadingFailures
#'
#' @return A Supply Availability Rate (SAR) between 0 and 1.
#' @export
#'
#' @importFrom igraph V
#' @importFrom igraph degree
#' @examples
#' ## Generate a theoretical supply network, define producers (P), intermediaries (I)
#' ## and distributors (D) vertices.
#' library(igraph)
#' g = make_tree(10)
#' V(g)[!degree(g, mode="in")]$type = "P"
#' V(g)[degree(g, mode="in") & degree(g, mode="in")]$type = "I"
#' V(g)[!degree(g, mode="out")]$type = "D"
#' V(g)$color = c("green","red","yellow")[factor(V(g)$type, levels=c("P","I","D"))]
#' plot(g)
#' ## Delete intermediary with highest betweenness and apply cascding disruptions
#' g2 = delete_vertices(g, V(g)[which.max(betweenness(g))])
#' g2 = applyCascadingFailures(g2)
#' plot(g2)
#' SAR(g, g2)
#'
SAR = function(g0, g)
{
  if (!length(V(g)[V(g)$type=="D"]))
    return(0)
  SA_g0 = length(V(g0)[V(g0)$type=="D" & degree(g0, mode="in")>0])
  SA_g  = length(V(g)[V(g)$type=="D"   & degree(g, mode="in")>0])
  SA_g / SA_g0
}

#' Analysis of supply network robustness
#'
#' This function performs a "targeted attack" of a graph or a "random failure"
#' analysis, calculating a tunable critical metric of the supply network after
#' successive edge or vertex removals. Attacks can target a vertex or an edge
#' according to different centrality indices.
#'
#' @details
#' At each time step, a vertex or an edge is deleted from the supply network.
#' Then, vertices isolated are also deleted (cascading failures), and critical
#' metric is recomputed. Process stops if:
#' * all graph elements has been deleted.
#' * critical metrics reached 0.
#' * maximal number of iterations has been reached
#'
#' @seealso Improves \code{\link[brainGraph:robustness]{brainGraph:robustness}} with more
#' modularity in the choice of centrality index and critical metric.
#' See https://github.com/cran/brainGraph/blob/master/R/robustness.R
#'
#' @param g igraph; a supply network where every node has a 'type'
#' attribute which can be "P" (producer), "I" (intermediary), or "D" (distributor).
#' @param type character string; either 'vertices' or 'edges' removals.
#' (default: 'vertices').
#' @param vs igraph.vs; used if type='vertices', the vertices which can be deleted.
#' (default: only intermediaries).
#' @param es igraph.es; used if type='edges', the edges which can be deleted.
#' (default: all graph edges).
#' @param mode character; how to choose the vertex or the edge to delete, either
#' 'random' or 'targeted'. If 'targeted', the v or e with highest betweenness
#' is selected (default: 'random').
#' @param metric.FUN function; critical supply network metric to recompute at
#' Each time step. (default: \code{\link{SAR}}).
#' @param niter numeric; maximal number of removals (default: infinite).
#' @param ...  if mode='targeted', other arguments can be passed to
#' \code{\link{getVertices}}, or \code{\link{getEdges}} functions. Example:
#' \itemize{
#'   \item if type='vertices' and ct.ind.FUN=\code{betweenness}, we target the
#'   vertex with highest betweenness.
#'   \item if type='edges' and ct.ind.FUN=\code{edge_betweenness}, we target
#'   the edge with highest betweenness.
#'   \item type='edges' and ct.ind.FUN=\code{betweenness} does not work.
#' }
#'
#' @return numeric between 0 and 1. The mean of \code{metric.FUN} over iterations.
#' @export
#'
#' @importFrom igraph V
#' @importFrom igraph E
#' @importFrom igraph delete_vertices
#' @importFrom igraph delete_edges
#' @examples
#' ## Generate a theoretical supply network, define producers (P),
#' ## Intermediaries (I) and distributors (D) vertices.
#' library(igraph)
#' set.seed(123)
#' g = make_tree(20)
#' V(g)[!degree(g, mode="in")]$type = "P"
#' V(g)[degree(g, mode="in") & degree(g, mode="in")]$type = "I"
#' V(g)[!degree(g, mode="out")]$type = "D"
#' V(g)$color = c("green","red","yellow")[factor(V(g)$type, levels=c("P","I","D"))]
#' ## Vertices robustness
#' robustnessGraph(g, type="v") # Random failures
#' robustnessGraph(g, type="v", mode="targ", ct.ind.FUN=betweenness) # Betweenness targeted attacks
#' robustnessGraph(g, type="v", mode="targ", ct.ind.FUN=degree) # Degree targeted
#' ## Edges robustness
#' robustnessGraph(g, type="e")  # Random failures
#' robustnessGraph(g, type="e", mode="targ") # Betweenness targeted attacks
#'
robustnessGraph = function(g,
                           type       = c("vertices", "edges"),
                           vs         = V(g)[V(g)$type=="I"],
                           es         = E(g),
                           mode       = c("random", "targeted"),
                           metric.FUN = SAR,
                           niter      = Inf,
                           ...)
{
  type = match.arg(type)
  mode = match.arg(mode)
  if (!is(metric.FUN, "function"))
    stop("metric.FUN require a function")
  # igraph uses 'name' as symbolic ids. Gives also names to vs or es
  if (type=="vertices") {
    if (is.null(V(g)$name))
      V(g)$name = as.character(seq(V(g)))
    if (!length(V(g)[V(g)$name %in% vs$name]))
      return(1)
  } else if (type=="edges") {
    if (is.null(E(g)$name))
      E(g)$name = as.character(seq(E(g)))
    if (!length(E(g)[E(g)$name %in% es$name]))
      return(1)
  }

  vs_names = vs$name # Store V and E to delete names
  es_names = es$name
  g_ref    = g    # Store reference graph
  metric_c = NULL # To be vector filled through iterations
  i        = 0

  repeat {
    # Delete a vertex or an edge,
    if (type=="vertices") {
      v_del = getVertices(g, vs=vs, mode=mode, ...)
      g = delete_vertices(g, v_del)
    } else if (type=="edges") {
      e_del = getEdges(g, es=es, mode=mode, ...)
      g = delete_edges(g, e_del)
    }
    # Delete other disrupted vertices and refresh list of vs / es
    g = applyCascadingFailures(g)
    vs = V(g)[V(g)$name %in% vs_names]
    es = E(g)[E(g)$name %in% es_names]
    # Recompute critical metric and add to output vector
    metric   = metric.FUN(g_ref, g)
    metric_c = c(metric_c, metric)
    i        = i + 1
    # Stop process if all elements deleted, or limits reached
    if (type=="vertices" && !length(vs))
      break
    if (type=="edges" && !length(es))
      break
    if (metric==0 || i>=niter)
      break
  }
  mean(metric_c)
}



#' Analysis of aggregated supply network robustness
#'
#' This function improves \code{\link{robustnessGraph}} by an aggregated approach
#' of vertices and edges deletions, applied to spatialized risks.
#' Vertices can be spatially concentrated in same regions (i.e. spatial entites),
#' thus subjects to same located risks. Similarly, logistic flows can use the
#' same roads, also subjects to located risks. Main vulnerabily points of a
#' spatial supply network can be crossroads zones on which depend the highest
#' betweenness edges. This function aims to explore spatial dimension of
#' supply network robustness, both in terms of vertices and edges disruptions.
#' The concept of aggregated graph robustness can be applied for other purposes
#' than spatial networks: vertices can be actors belonging to same organizations
#' facing global disruptions for example.
#'
#' @details
#' This function extends \code{\link{robustnessGraph}} with a supplementary
#' parameter \code{mat}, corresponding to a matrix of adjacency between
#' vertices or edges in rows, and spatial entities in columns.
#' Relation between graph elements (V or E) and spatial entites can be represented
#' as a bipartite graph. In further version, matrix will be replaced by an adjacency
#' list.
#'
#' @inheritParams robustnessGraph
#' @param mat matrix, matrix of adjacency between vs or es to delete and entity.
#' @param ct.ind.FUN function; centraltity index used to targete v or e.
#' Sum of centrality indices by entity is used.
#'
#' @return numeric between 0 and 1. The mean of \code{metric.FUN} over iterations.
#' @export
#'
#' @importFrom igraph V
#' @importFrom igraph E
#' @importFrom igraph delete_vertices
#' @importFrom igraph delete_edges
#' @examples
## Generate a theoretical supply network, define producers (P),
## Intermediaries (I) and distributors (D) vertices.
#' library(igraph)
#' set.seed(123)
#' g = make_tree(20)
#' V(g)[!degree(g, mode="in")]$type = "P"
#' V(g)[degree(g, mode="in") & degree(g, mode="in")]$type = "I"
#' V(g)[!degree(g, mode="out")]$type = "D"
#' V(g)$color = c("green","red","yellow")[factor(V(g)$type, levels=c("P","I","D"))]
#' plot(g)
#' ## Build aggregation matrix
#' V(g)$name = as.character(seq(V(g)))
#' V(g)$agg_id = sample(letters[1:5], length(V(g)), replace = TRUE)
#' mat = xtabs(data=data.frame(v_name=V(g)$name, entity_id=V(g)$agg_id))
#' ## Compare number of vertices by entity and the sums of their betweenness
#' colSums(mat)
#' colSums(mat * betweenness(g, rownames(mat)))
#' ## Aggregated modes causes more damages in targeted mode
#' robustnessGraph(g,  type="v", mode="targ")
#' robustnessGraphAgg(g, mat=mat, type="v", mode="targ")
#'
robustnessGraphAgg = function(g,
                              mat,
                              type       = c("vertices", "edges"),
                              vs         = V(g)[V(g)$type=="I"],
                              es         = E(g),
                              mode       = c("random", "targeted"),
                              ct.ind.FUN = betweenness,
                              metric.FUN = SAR,
                              niter      = Inf)
{
  type = match.arg(type)
  mode = match.arg(mode)
  if (!is(metric.FUN, "function"))
    stop("metric.FUN require a function")
  # Change functions used through the process according to type of deleted
  # elements. 've' stands for 'vertices or edges'
  if (type=="vertices") {
    ves_names  = names(vs)
    VE         = V
    delete.FUN = delete_vertices
  } else if (type=="edges") {
    ves_names  = names(es)
    VE         = E
    delete.FUN = delete_edges
  }
  # Elements to delete must match agg mat rows
  if (any(!ves_names %in% rownames(mat)))
    stop("Elements are missing in mat rownames")
  m = mat[ves_names, ]

  g_ref    = g
  i        = 0
  metric_c = NULL

  repeat {
    # Choose 1 entity to delete, random or with highest sum of centrality indices
    if (mode=="random") {
      x_del = sample(colnames(m), 1)
    } else if (mode=="targeted") {
      m_ct = m * ct.ind.FUN(g, rownames(m)) # Mulitply each "1" by edge betweenness for example
      x_del = colnames(m)[which.max(colSums(m_ct))]
    }
    # Delete vertices or edges adjacent to x_ded, and then vertices impacted by this disruption
    ve_del = rownames(m[m[, x_del]>0, , drop=FALSE])
    #   print(length(ve_del))
    g = delete.FUN(g, ve_del)
    g = applyCascadingFailures(g)
    # Recompute critical metric of SAR and add to vector result
    metric   = metric.FUN(g_ref, g)
    metric_c = c(metric_c, metric)
    i     = i + 1
    # Clean agg matrix from deleted v or e and empty entities
    ve2del_left = VE(g)$name[VE(g)$name %in% ves_names]
    m = m[ve2del_left,  , drop=FALSE]
    m = m[, colSums(m)>0, drop=FALSE]
    # Stop process if no more elements to delete left, or limits reached
    if (metric==0 || i==niter || !length(ve2del_left))
      break
  }
  mean(metric_c)
}

