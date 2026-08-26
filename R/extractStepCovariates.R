#' Extract Habitat Covariates for Random Steps
#'
#' Extracts covariate values at the coordinates of each step in a
#' \code{random_steps_prop} object. For numeric layers, values are extracted
#' directly at the endpoint. For image layers, kernel-aggregated values are
#' computed via an internal kernel-extraction helper defined at the top of
#' this function. Column names are taken from the layer names of each
#' \code{SpatRaster}. Coordinates that lie outside a layer's extent are set
#' to \code{NA} and a warning is issued.
#'
#' @param x An object of class \code{random_steps_prop} as returned by
#'   \code{\link{createRandomSteps}}.
#' @param numericLayers \code{SpatRasterCollection} or \code{NULL}. Continuous
#'   or binary raster layers. Values are extracted at the step endpoint
#'   (\code{x2_}, \code{y2_}). Coordinates outside a layer's extent are
#'   returned as \code{NA} with a warning. If \code{scaleNumericLayers = TRUE},
#'   each layer is scaled to (0, 1) before extraction.
#' @param imageLayers \code{SpatRasterCollection} or \code{NULL}. Raster layers
#'   evaluated via a spatial kernel centred on the step endpoint. Coordinates
#'   outside a layer's extent are returned as \code{NA} with a warning. If
#'   \code{scaleImageLayers = TRUE}, each layer is scaled to (0, 1) before
#'   extraction.
#' @param imageKernelDimensions odd positive integer vector. Spatial kernel size
#'   (in pixels) for each image layer. Must be odd and have one entry per image
#'   layer. Default: \code{c(3)}.
#' @param imageKernelFunctions list of functions. Aggregation function applied
#'   to each kernel window (e.g. \code{mean}, \code{max}). Must have one entry
#'   per image layer. Default: \code{list(f1 = mean)}.
#' @param scaleNumericLayers logical. If \code{TRUE} (default), each numeric
#'   layer is rescaled to (0, 1) based on its observed min/max before
#'   extraction.
#' @param scaleImageLayers logical. If \code{TRUE} (default), each image layer
#'   is rescaled to (0, 1) based on its observed min/max before extraction.
#'
#' @return An object of class \code{steps_covariates} (and all classes inherited
#'   from \code{x}) with one additional column per layer added.
#'   Column names match the names of each layer.
#'
#' @details
#' All layers must share the same CRS.
#'
#' @seealso \code{\link{createRandomSteps}}
#'
#'
#' @example inst/examples/dataprep-example.R
#'
#' @export
#' @author Mika Schubert


extractStepCovariates <- function(x,
                                  numericLayers         = NULL,
                                  imageLayers           = NULL,
                                  imageKernelDimensions = c(3),
                                  imageKernelFunctions  = list(f1 = mean),
                                  scaleNumericLayers    = TRUE,
                                  scaleImageLayers      = TRUE) {

  # Internal helper functions ---------------------------------------------
  # cropScaleRaster() and extractKernelValues() are also used by
  # simulateTrack() (see simulation.R), but since local/nested functions are
  # not visible outside the function they are defined in, they are defined
  # again here as local copies - together with checkExtent(), which is only
  # ever needed inside extractStepCovariates().

  # Crops a raster layer to a given extent and optionally rescales it to (0, 1).
  #   layer - SpatRaster to process
  #   ext   - SpatExtent to crop to
  #   scale - if TRUE, rescale cropped values to (0, 1); skipped if the layer
  #           is constant (min == max), since dividing by zero would occur
  cropScaleRaster <- function(layer, ext, scale = TRUE) {
    r <- terra::crop(layer, ext)
    terra::ext(r) <- ext
    if (scale) {
      r_min <- terra::global(r, "min", na.rm = TRUE)[1, 1]
      r_max <- terra::global(r, "max", na.rm = TRUE)[1, 1]
      if (r_max > r_min) r <- (r - r_min) / (r_max - r_min)
    }
    r
  }

  # For each coordinate pair (xs[i], ys[i]), extracts the raster values inside
  # a square window of kernelDim x kernelDim pixels centred on that point and
  # aggregates them with kernelFun (e.g. mean, max). Raster borders are
  # wrapped toroidally, i.e. a window that runs off one edge of the raster
  # continues on the opposite edge.
  #   raster    - SpatRaster to read from
  #   xs, ys    - coordinates of the query points (same length)
  #   kernelDim - odd side length (in pixels) of the square window
  #   kernelFun - aggregation function applied to the values in each window
  extractKernelValues <- function(raster, xs, ys, kernelDim, kernelFun) {
    res_x <- terra::res(raster)[1]
    res_y <- terra::res(raster)[2]
    half  <- floor(kernelDim / 2)

    border  <- terra::ext(raster)
    x_range <- border[2] - border[1]
    y_range <- border[4] - border[3]

    offsets <- expand.grid(dx = seq(-half, half),
                           dy = seq(-half, half))
    n_pts <- length(xs)
    n_off <- nrow(offsets)

    all_x <- rep(xs, each = n_off) +
      rep(offsets$dx, times = n_pts) * res_x
    all_y <- rep(ys, each = n_off) +
      rep(offsets$dy, times = n_pts) * res_y

    all_x <- ((all_x - border[1]) %% x_range) + border[1]
    all_y <- ((all_y - border[3]) %% y_range) + border[3]

    vals_all <- terra::extract(raster, cbind(all_x, all_y))[, 1]
    vals_mat <- matrix(vals_all, nrow = n_pts,
                       ncol = n_off, byrow = TRUE)

    apply(vals_mat, 1, function(row) {
      v <- row[!is.na(row)]
      if (length(v) == 0) NA_real_ else kernelFun(v)
    })
  }

  # Warns if any coordinates lie outside the extent of a given raster layer.
  #   xs, ys - coordinates to check
  #   raster - SpatRaster whose extent is checked against
  # Rueckgabe: logical vector, TRUE where the coordinate lies outside.
  checkExtent <- function(xs, ys, raster) {
    ext      <- terra::ext(raster)
    lyr_name <- names(raster)
    outside  <- xs < ext[1] | xs > ext[2] | ys < ext[3] | ys > ext[4]
    if (any(outside, na.rm = TRUE))
      warning(sum(outside, na.rm = TRUE),
              " coordinate(s) out of layer '", lyr_name, "' extent; set to NA.",
              call. = FALSE)
    outside}


  # Validation:

  if (!inherits(x, c("random_steps_prop","steps_covariates") ))
    stop("`x` must be an object of class 'random_steps_prop' or 'steps_covariates'.")
  if (is.null(numericLayers) && is.null(imageLayers))
    stop("At least one of `numericLayers` or `imageLayers` must be provided.")
  if (!is.null(numericLayers)) {
    if (!inherits(numericLayers, "SpatRasterCollection"))
      stop("`numericLayers` must be of class 'SpatRasterCollection'.")}
  if (!is.null(imageLayers)) {
    if (!inherits(imageLayers, "SpatRasterCollection"))
      stop("`imageLayers` must be of class 'SpatRasterCollection'.")
    nImage <- length(terra::as.list(imageLayers))
    if (length(imageKernelDimensions) != nImage)
      stop("`imageKernelDimensions` must have one entry per image layer.")
    if (length(imageKernelFunctions) != nImage)
      stop("`imageKernelFunctions` must have one entry per image layer.")
    if (any(imageKernelDimensions %% 2 == 0))
      stop("All `imageKernelDimensions` must be odd numbers.")}

  # CRS check
  rasterList <- c(
    if (!is.null(numericLayers)) terra::as.list(numericLayers) else list(),
    if (!is.null(imageLayers))   terra::as.list(imageLayers)   else list())
  if (length(rasterList) > 1) {
    crsList  <- sapply(rasterList, terra::crs)
    nonEmpty <- crsList[nchar(crsList) > 0]
    if (length(nonEmpty) > 1 && length(unique(nonEmpty)) > 1)
      stop("All layers must have the same CRS.")}


  xs <- x$x2_
  ys <- x$y2_


  # Extract numeric layer values:

  if (!is.null(numericLayers)) {
    numericRasterList <- terra::as.list(numericLayers)

    for (k in seq_along(numericRasterList)) {
      r        <- cropScaleRaster(numericRasterList[[k]],
                                  ext   = terra::ext(numericRasterList[[k]]),
                                  scale = scaleNumericLayers)
      lyr_name <- names(r)
      outside  <- checkExtent(xs, ys, r)
      vals     <- terra::extract(r, cbind(xs, ys))[, 1]
      vals[outside] <- NA_real_
      x[[lyr_name]] <- vals
    }
  }


  # Extract image layer kernel values

  if (!is.null(imageLayers)) {
    imageRasterList <- terra::as.list(imageLayers)

    for (k in seq_along(imageRasterList)) {
      r        <- cropScaleRaster(imageRasterList[[k]],
                                  ext   = terra::ext(imageRasterList[[k]]),
                                  scale = scaleImageLayers)
      lyr_name <- names(r)
      outside  <- checkExtent(xs, ys, r)

      vals           <- rep(NA_real_, length(xs))
      inside_idx     <- which(!outside)
      if (length(inside_idx) > 0) {
        vals[inside_idx] <- extractKernelValues(
          raster    = r,
          xs        = xs[inside_idx],
          ys        = ys[inside_idx],
          kernelDim = imageKernelDimensions[k],
          kernelFun = imageKernelFunctions[[k]]
        )
      }
      x[[lyr_name]] <- vals
    }
  }


  # New class

  class(x) <- c("steps_covariates", setdiff(class(x), "steps_covariates"))
  x
}

