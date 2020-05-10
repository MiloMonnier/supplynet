
#' Create a multiscale grid
#'
#' @param sf sf class; the spatial object grid cells will cover.
#' @param xref numeric; x coordinate of reference pixel bottomleft corner.
#' If NULL (default), the center of the extent is taken.
#' @param yref numeric; y coordinate of reference pixel bottomleft corner.
#' If NULL (default), the center of the extent is taken.
#' @param scales numeric; resolution of the raster cells, expressed in units of
#' \code{extent} CRS. If CRS is EPSG:2154, scale is in meters. If EPSG:4326,
#' it is in decimal degrees.
#' @param buffers sf class; a list of buffers.
#'
#' @return a sf class grid.
#' @export
#'
#' @importFrom methods as
#' @importFrom sf st_centroid
#' @importFrom sf st_crs
#' @importFrom sf st_intersects
#' @importFrom sf st_set_crs
#' @importFrom spex polygonize
#' @importFrom stars st_rasterize
#' @importFrom raster resample
#' @importFrom raster crop
#' @importFrom raster mask
#' @examples
#' library(sf)
#' ## Open Sein river data
#' data(seine, package="spData")
#' plot(st_geometry(seine))
#' ## Create a point for Paris
#' xref = 652215.52
#' yref = 6861681.77
#' centro = st_sf(st_sfc(st_point(c(xref, yref)), crs=2154))
#' plot(st_geometry(centro), col="red", pch=19, add=TRUE)
#' # Create a list of 3 buffers of 20, 100 and 500km
#' radius = c(50000, 100000, 200000)
#' radius = rev(radius)
#' buffers = lapply(radius, function(r) st_buffer(centro, dist=r))
#' lapply(buffers, function(x) plot(st_geometry(x), border="blue", add=TRUE))
#' # Set grid cells scales of 1, 10 and 50km, and create multiscale grid
#' scales = c(5000, 10000, 20000)
#' scales = rev(scales)
#' grid = multiScaleGrid(sf=seine, xref, yref, scales, buffers)
#' plot(st_geometry(grid), col=NA, add=TRUE)
#'
multiScaleGrid = function(sf,
                          xref = NULL,
                          yref = NULL,
                          scales,
                          buffers)
{
  if (length(scales) != length(buffers))
    stop("scales and buffers must be same length")
  if (!is(buffers,"list") || any(!sapply(buffers,is,"sf")))
    stop("buffers must be a list of sf objects")

  # Rasterize spatial object with a reference raster, and vectorize to make a grid
  rast     = as(stars::st_rasterize(sf), "Raster")
  rast_ref = createRaster(sf, xref, yref, scale=scales[1])
  grid     = raster::resample(rast, rast_ref) %>%
    spex::polygonize() %>%
    st_set_crs(st_crs(sf))

  for (i in 2:length(scales)) { # First buffer is useless

    # Get grid cells intersecting current polygon, and dissolve these cells in 1
    grid_int_ids    = unlist(st_intersects(buffers[[i]] , st_centroid(grid)))
    grid_int_poly   = grid[grid_int_ids, ]
    grid_int_poly_u = aggregate(grid_int_poly, by=list(rep(1,nrow(grid_int_poly))), FUN=length) # st_union fails

    # Make a new raster of smaller resolution and keep only the part into polygon
    rast_ref = createRaster(sf, xref, yref, scale=scales[i])
    grid_int = resample(rast, rast_ref) %>%
      crop(grid_int_poly_u) %>%
      mask(grid_int_poly_u) %>%
      polygonize() %>%
      st_set_crs(st_crs(sf))

    # Delete wider scales cells from big grid and bind the new with smaller resolution
    grid = grid[-grid_int_ids, ]
    grid = rbind(grid[, c("layer","geometry")],
                 grid_int[, c("layer","geometry")])
  }

  grid$id = seq(nrow(grid))
  grid    = grid[, c("id","geometry")]
}

