
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
#' g = erdos.renyi.game(100, 120, 'gnm')
#' for (i in 1:100)
#'   g = rewireEdge(g)
#'
rewireEdge = function(g,
                      edge        = sample(E(g), 1),
                      what        = c("origin", "destination"),
                      vs           = NULL,
                      multi.edges = FALSE,
                      ...)
{
  if (!is(edge,"igraph.es") || length(edge) != 1)
    stop("edge require an igraph.es class object of length 1")
  if (!is.null(vs) && !is(vs,"igraph.vs"))
    stop("vs require a igraph.vs class object")
  what = sample(what, 1)
  # Get extremities of the edge to rewire
  vi = ends(g, edge, names=TRUE)[, 1][1] # If any double egdes, keep only 1st
  vj = ends(g, edge, names=TRUE)[, 2][1]
  # Filter potential new origin or destinations
  if (is.null(vs)) {
    vs = V(g)
    if (what=="origin") {
      vs = vs[vs$type %in% c("P","I")]
    } else if (what=="destination") {
      vs = vs[vs$type %in% c("I","D")]
    }
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
  vk = getVertices(g, n=1, vs=vs, ...)
  # vk = getVertices(g, n=1, v=v, mode="random")
  # Generate new edge
  if (what=="origin") {
    new_e = c(vk, vj)
  } else if (what=="destination") {
    new_e = c(vi, vk)
  }
  # Replace the old by the new edge
  g = delete_edges(g, edge)
  g = add_edges(g, new_e)
  name = paste(new_e, collapse="_")
  type = paste0(V(g)[new_e[1]]$type, V(g)[new_e[2]]$type)
  attr = list(name=name, type=type)
}

# rm(list=ls())
# load("outputs/3-network_spatial_cleaned.RData")
# source("R/getVertices.R")
# edge        = sample(E(g), 1)
# what        = c("origin", "destination")
# v           = NULL
# multi.edges = FALSE
# iie.ceil    = NULL


# g = updateGraphAttributes(g, e=c("name","type","sc"))
# new_e  = names(new_e)

# e_sc    = ifelse(e_type=="PD", "short","long")
# g = add_edges(g, new_e)
# g = add_edges(g, new_e, attr=list(name=new_e_name, type=new_e_type))
# Avoid to create forbidden edges: double edges or excessive II edges
# if (!is.null(iie.ceil)  &&  e_type=="II"  &&  length(V(g)[type=="II"]) >= iie.ceil) {
#   if (verbose)
#     message(paste("rewireEdge() - Cancel: number of Inter-->Inter edges ceiled to", iie.ceil))
#   return(g)
# }




# for (i in 1:1000) {
#   print(i)
#   g = rewireEdge(g, sample(E(g), 1))
#   if (length(E(g)[which_multiple(g, E(g))]))
#     warning("Multiedges")
#   if (length(E(g)[which_loop(g, E(g))]))
#     warning("Loops")
#   if (length(V(g)[degree(g)==0]))
#     warning("Isolated vertex")
#   if (length(V(g)[type=="I" & degree(g, mode="in")==0]))
#     warning("Intermediary with 0 supplier")
#   if (length(V(g)[type=="I" & degree(g, mode="out")==0]))
#     warning("Intermediary with 0 customer")
# }

