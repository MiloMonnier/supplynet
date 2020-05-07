#' Creating spatial vertices or edges from igraph object
#'
#' Extends \code{\link[igraph]{get.data.frame}} by adding spatial dimension with \code{\link[sf]{st_as_sf}}
#'
#' @param g An igraph object
#' @param what Character wether to return spatial "vertices" or spatial "edges. Default is "vertices"
#' @param coords names of the numeric columns holding coordinates in vertices dataframe
#' @param crs Coordinate reference system: integer with the EPSG code, or character with proj4string
#'
#' @return a sf class object: point if type = "vertices", linestring if "edges".
#' @export
#'
#' @import igraph
#' @import sf
#' @examples
#' \donttest{
#' library(igraph)
#' library(sf)
#' data(USairports, package="igraphdata")
#' g = USairports
#' url = 'https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat'
#' apts = read.csv(url, header=FALSE)
#' V(g)$lng = apts[match(V(g)$name, apts[,5]), 8]
#' V(g)$lat = apts[match(V(g)$name, apts[,5]), 7]
#' g = delete.vertices(g, which(is.na(V(g)$lat)))
#' sf_e = igraph2sf(g, what="e", coords=c("lng", "lat"), crs=4326)
#' sf_v = igraph2sf(g, what="v", coords=c("lng", "lat"), crs=4326)
#' plot(st_geometry(sf_e[sample(seq(nrow(sf_e), 50)), ]), col="blue")
#' plot(st_geometry(sf_v), col="red", add=TRUE)
#' }
#'
igraph2sf = function(g,
                     what   = c("vertices", "edges"),
                     coords = c("x","y"),
                     crs    = 2154)
{
  what = match.arg(what)
  # Get vertices and edges dataframes
  v = get.data.frame(g, "vertices")
  e = get.data.frame(g, "edges")

  if (what=="vertices") {
    # Convert vertices to sf class object and re-attach lost columns
    sf_v = st_as_sf(v, coords=coords, crs=crs)
    sf_v$x   = v$x
    sf_v$y   = v$y
    sf_v$lng = v$lng
    sf_v$lat = v$lat
    sf_v = sf_v[, c(colnames(v), "geometry")]
    return(sf_v)

  } else if (what=="edges") {
    # Retrieve edges origin and destinations coordinates from vertices attributes
    i = ends(g, E(g), names=FALSE)[, 1]
    j = ends(g, E(g), names=FALSE)[, 2]
    xi = v[, coords[1]][i]
    yi = v[, coords[2]][i]
    xj = v[, coords[1]][j]
    yj = v[, coords[2]][j]
    orig = cbind(xi, yi)
    dest = cbind(xj, yj)
    # Generate edges lines geometry and make it sf object
    e$geometry = do.call("c",
                         lapply(seq(nrow(orig)), function(i) {
                           st_sfc(st_linestring(as.matrix(rbind(orig[i,], dest[i,]))),
                                  crs=crs)
                         }))
    sf_e = st_as_sf(e)
    return(sf_e)
  }
}
