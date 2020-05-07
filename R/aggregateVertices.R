
#' Aggregate vertices of a graph
#'
#' Aggregate vertices of an igraph object. For vertices numeric columns, mean of groups is computed. For character class
#' columns, uniques values are pasted.
#'
#' @param g An igraph object
#' @param by Vector of the same length than vertices to aggregate them.
#' @param v.agg igraph.vs or character; the vertices to aggregate. Default is
#' all graph vertices.
#'
#' @return An igraph object.
#' @export
#'
#' @importFrom igraph V
#' @importFrom igraph E
#' @importFrom igraph get.data.frame
#' @importFrom igraph graph_from_data_frame
#' @importFrom stats aggregate
#' @examples
#' library(igraph)
#' g = erdos.renyi.game(100, 120, 'gnm')
#' plot(g)
#' by = sample(letters[1:10], length(V(g)), replace=TRUE)
#' g2 = aggregateVertices(g, by)
#' plot(g2)
#'
aggregateVertices = function(g,
                             by,
                             v.agg = V(g))
{
  if (length(by) != length(V(g)))
    stop("by require a vector of same length than vertices number")
  if (is.null(V(g)$name))
    V(g)$name = as.vector(V(g))
  if (is(v.agg, "igraph.vs"))
    v.agg = names(V(g)[v.agg])

  # Aggregate only the vertices passed in v.agg
  v = get.data.frame(g, "vertices")
  # by = as.vector(v.agg)
  by = ifelse(v$name %in% v.agg, by, v$name)
  # Split df in 2 according to columns classes and apply different functions
  df = v
  df = df[, sapply(df, class) %in% c("numeric", "character"), drop=FALSE]
  # if ()
  dfnum = df[, sapply(df, class)=="numeric",   drop=FALSE]
  dfchr = df[, sapply(df, class)=="character", drop=FALSE]
  dfnum = aggregate(dfnum, by=list(by), FUN=mean)
  dfchr = aggregate(dfchr, by=list(by), FUN=function(x) toString(sort(unique(x))))
  # Merge the 2 df and reorder as original
  v_agg = data.frame(dfnum, dfchr)
  rownames(v_agg) = v_agg[, 1]
  v_agg = v_agg[, colnames(df), drop=FALSE]
  # v_agg = v_agg[,c("name", colnames(df)), drop=FALSE]
  v_agg = data.frame(name=rownames(v_agg), v_agg)
  # Update edges origin and destination IDs and aggregate edges
  eij = ends(g, E(g))
  from = by[match(eij[, 1], v$name)]
  to   = by[match(eij[, 2], v$name)]
  e = data.frame(from=from, to=to, weight=1)
  e_agg = aggregate(weight ~ from+to, e, FUN="sum")

  # Rebuild the graph
  g = graph_from_data_frame(e_agg, v_agg, directed=TRUE)
}
