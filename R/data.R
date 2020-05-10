#' Spatial network of Montpellier city supply in tomato
#'
#' 155 nodes (vertices) and 228 links (edges).
#' x and y vertices attributes contains spatial coordinates in RGF93 Coordinates
#' Reference System EPSG:2154.
#'
#' @docType data
#'
#' @usage data(tomato_anonym)
#'
#' @format An object of class \code{"igraph"}; see \code{\link[igraph]{igraph}}.
#'
#' @keywords datasets, graph, supply network, spatial network
#'
#'
#'
#' @examples
#' ## Load data and rename graph as "g"
#' library(igraph)
#' data(tomato_anonym)
#' g = tomato_anonym
#' ## Inspect vertices and edges dataframes
#' v = get.data.frame(g, "vertices")
#' e = get.data.frame(g, "edges")
#' str(v)
#' str(e)
#' ## Check geographical cooordinates
#' ## By default,  igraph uses V(g)$x and V(g)$y as layout coordinates
#' ## We don't need conversion to view the spatial network
#' plot(g)
#'
"tomato_anonym"
