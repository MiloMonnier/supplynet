
# CREATE RASTER -----------------------------------------------------------

#' Creating customized rasters
#'
#' Creates a RasterLayer class object covering a given sf object extent.
#' Resolution (scale) is passed in kilometers. Raster pixels can be fitted on
#' a reference point passed with xref and yref arguments.
#'
#' @param extent sf or sp; spatial object defining the extent covered by raster.
#' Spatial obhect can be points, lines or polygons.
#' @param xref numeric; x coordinate of reference pixel bottomleft corner.
#' If NULL (default), the center of the extent is taken.
#' @param yref numeric; y coordinate of reference pixel bottomleft corner.
#' If NULL (default), the center of the extent is taken.
#' @param scale numeric; resolution of the raster cells, expressed in units of
#' \code{extent} CRS. If CRS is EPSG:2154, scale is in meters. If EPSG:4326,
#' it is in decimal degrees.
#'
#' @return a RasterLayer object with same CRS than \code{extent}.
#' @export
#'
#' @importFrom methods is
#' @importFrom sf st_bbox
#' @importFrom sf st_as_sf
#' @importFrom sp CRS
#' @importFrom raster raster
#' @importFrom raster crs
#' @examples
#' \donttest{
#' # Create rasters covering French Seine river basin, fitted on "Point Zero"
#' # of french roads in Paris city center.
#' library(sf)
#' library(spData)
#' library(raster)
#' data(seine, package="spData")
#' pt_zero = st_sf(st_sfc(st_point(c(652215.52, 6861681.77)), crs=2154))
#' coords = st_coordinates(pt_zero)
#' rast = createRaster(extent=seine, xref=coords[1], yref=coords[2], scale=100)
#' values(rast) = rnorm(ncell(rast))
#' plot(rast)
#' plot(st_geometry(seine), col="blue", add=TRUE)
#' plot(st_geometry(pt_zero), col="red", add=TRUE)
#' # Test with sp class in WGS84 CRS, without specifying xref and yref
#' library(sp)
#' data(SplashDams, package="spData")
#' proj4string(SplashDams) = CRS("+proj=longlat +datum=WGS84")
#' rast = createRaster(SplashDams, scale=0.0001)
#' values(rast) = rnorm(ncell(rast))
#' plot(rast)
#' plot(SplashDams, col="blue", add=TRUE)
#' }
#'
createRaster = function(extent,
                        xref   = NULL,
                        yref   = NULL,
                        scale  = 1)
{
  options(scipen=100000) # Avoid scientific notation for large scale numbers
  if (is(extent, "Spatial")) # From sp to sf if necessary
    extent = st_as_sf(extent)
  if (!is(extent, "sf"))
    stop("sf.extent require a sf class object")

  # Get spatial object limits
  bbox = st_bbox(extent)
  xmin = bbox["xmin"]
  ymin = bbox["ymin"]
  xmax = bbox["xmax"]
  ymax = bbox["ymax"]
  # xref and yref set reference pixel bottomleft corner. Ose extent center by default
  if (is.null(xref))
    xref = mean(xmin, xmax)
  if (is.null(yref))
    yref = mean(ymin, ymax)
  # Compute number of necessary columns and rows to fit sf extent
  # scale = scale.km * 1000 # From km to meters
  ncolL = ceiling((xref - xmin) / scale)
  ncolR = ceiling((xmax - xref) / scale)
  nrowB = ceiling((yref - ymin) / scale)
  nrowT = ceiling((ymax - yref) / scale)
  ncol  = ncolL + ncolR
  nrow  = nrowB + nrowT
  # Get new fitted raster limits
  newxmin = xref - (ncolL * scale)
  newxmax = xref + (ncolR * scale)
  newymin = yref - (nrowB * scale)
  newymax = yref + (nrowT * scale)
  # Create the raster
  rast = raster(ncol=ncol, nrow=nrow,
                xmn=newxmin, xmx=newxmax,
                ymn=newymin, ymx=newymax,
                crs=CRS(crs(extent)),
                vals=0)
}
