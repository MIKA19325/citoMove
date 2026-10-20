#' Extract habitat covariates at step end points
#'
#' Adds one column per raster layer to a set of observed and random steps. For
#' numeric layers, the value of the cell at the step end point (`x2_`, `y2_`)
#' is extracted. For image layers, the cell values within a square window
#' (kernel) around the end point are aggregated to a single value per step,
#' e.g. their mean. The result is the tabular input for [dnn_ssf()] or for a
#' conditional logistic regression.
#'
#' @param x An object of class `random_steps_prop`, as returned by
#'   [createRandomSteps()], or of class `steps_covariates`, as returned by a
#'   previous call of this function or of [extractStepImagekernels()]. This
#'   allows adding covariates in several calls.
#' @param numericLayers `NULL` or a `SpatRasterCollection` (see
#'   [terra::sprc()]). The value of the cell at each step end point is
#'   extracted. The layer names (see [names()]) are used as column names.
#' @param imageLayers `NULL` or a `SpatRasterCollection`. The cell values in a
#'   square window around each step end point are aggregated with the
#'   corresponding function in `imageKernelFunctions`. The layer names are used
#'   as column names.
#' @param imageKernelDimensions Vector of odd positive integers, one per layer
#'   in `imageLayers`. Side length of the window in cells (e.g. `3` gives a
#'   3 x 3 window).
#' @param imageKernelFunctions List of functions, one per layer in
#'   `imageLayers`. Each function takes a numeric vector (the cell values in
#'   the window) and returns a single number, e.g. `mean` or `max`.
#' @param scaleNumericLayers Logical. If `TRUE` (default), each numeric layer
#'   is rescaled to the range 0 to 1 using its minimum and maximum before
#'   extraction, so that coefficients are comparable across layers.
#' @param scaleImageLayers Logical. If `TRUE` (default), each image layer is
#'   rescaled to the range 0 to 1 in the same way.
#'
#' @return An object of class `steps_covariates` (in addition to the classes of
#'   `x`) with one numeric column per layer, named after the layer. Values are
#'   `NA` where the step end point lies outside the extent of the layer.
#'
#' @details
#' **Names.** Layer names must be unique and should not coincide with existing
#' columns of `x`, which would be overwritten.
#'
#' **CRS.** All layers must share the same CRS. The coordinates of `x` are
#' assumed to be in this CRS; this is not checked.
#'
#' **Out-of-extent steps.** End points outside the extent of a layer are set
#' to `NA` with a warning; coordinates are not wrapped. Windows of image layers
#' that extend beyond the edge of a layer are completed with cells from the
#' opposite edge (toroidal wrapping), consistent with [simulateTrack()]. `NA`
#' cells inside a window are ignored by the aggregation. Because [dnn_ssf()]
#' does not accept missing values, remove complete strata (all rows with the
#' same `step_id_`) that contain `NA` before fitting.
#'
#' **Scaling.** Each layer is rescaled using the minimum and maximum over its
#' full extent. [simulateTrack()] instead rescales over the shared extent of
#' all layers; values are therefore only identical between both functions if
#' all layers have the same extent.
#'
#' **Internal helpers**, defined at the top of the function body:
#' - `cropScaleRaster()` optionally rescales a layer to the range 0 to 1.
#' - `extractKernelValues()` reads the square window of cells around each end
#'   point and aggregates it with the kernel function.
#' - `checkExtent()` flags end points outside the extent of a layer and issues
#'   the warning.
#'
#' @example inst/examples/dataprep-example.R
#'
#' @seealso [createRandomSteps()], [extractStepImagekernels()], [dnn_ssf()]
#'
#' @author Mika Schubert
#' @export

extractStepCovariates <- function(x,
                                  numericLayers         = NULL,
                                  imageLayers           = NULL,
                                  imageKernelDimensions = c(3),
                                  imageKernelFunctions  = list(f1 = mean),
                                  scaleNumericLayers    = TRUE,
                                  scaleImageLayers      = TRUE) {

  # Helper function: Crops a raster layer to a given extent and optionally
  # rescales it to (0, 1).
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

  # Helper function: For each coordinate pair (xs[i], ys[i]), extracts the
  # raster values inside a square window of kernelDim x kernelDim pixels
  # centred on that point and aggregates them with kernelFun (e.g. mean, max).
  # Raster borders are wrapped toroidally.
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

  # Helper function: Warns if any coordinates lie outside the extent of a
  # given raster layer.
  #   xs, ys - coordinates to check
  #   raster - SpatRaster whose extent is checked against
  # Returns: logical vector, TRUE where the coordinate lies outside.
  checkExtent <- function(xs, ys, raster) {
    ext      <- terra::ext(raster)
    lyr_name <- names(raster)
    outside  <- xs < ext[1] | xs > ext[2] | ys < ext[3] | ys > ext[4]
    if (any(outside, na.rm = TRUE))
      warning(sum(outside, na.rm = TRUE),
              " coordinate(s) out of layer '", lyr_name, "' extent; set to NA.",
              call. = FALSE)
    outside
  }


  # Validation:
  if (!inherits(x, c("random_steps_prop", "steps_covariates")))
    stop("`x` must be an object of class 'random_steps_prop' or 'steps_covariates'.")
  if (is.null(numericLayers) && is.null(imageLayers))
    stop("At least one of `numericLayers` or `imageLayers` must be provided.")
  if (!is.null(numericLayers)) {
    if (!inherits(numericLayers, "SpatRasterCollection"))
      stop("`numericLayers` must be of class 'SpatRasterCollection'.")
  }
  if (!is.null(imageLayers)) {
    if (!inherits(imageLayers, "SpatRasterCollection"))
      stop("`imageLayers` must be of class 'SpatRasterCollection'.")
    nImage <- length(terra::as.list(imageLayers))
    if (length(imageKernelDimensions) != nImage)
      stop("`imageKernelDimensions` must have one entry per image layer.")
    if (length(imageKernelFunctions) != nImage)
      stop("`imageKernelFunctions` must have one entry per image layer.")
    if (any(imageKernelDimensions %% 2 == 0))
      stop("All `imageKernelDimensions` must be odd numbers.")
  }

  # CRS check:
  rasterList <- c(
    if (!is.null(numericLayers)) terra::as.list(numericLayers) else list(),
    if (!is.null(imageLayers))   terra::as.list(imageLayers)   else list())
  if (length(rasterList) > 1) {
    crsList  <- sapply(rasterList, terra::crs)
    nonEmpty <- crsList[nchar(crsList) > 0]
    if (length(nonEmpty) > 1 && length(unique(nonEmpty)) > 1)
      stop("All layers must have the same CRS.")
  }

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


  # Extract image layer kernel values:
  if (!is.null(imageLayers)) {
    imageRasterList <- terra::as.list(imageLayers)

    for (k in seq_along(imageRasterList)) {
      r        <- cropScaleRaster(imageRasterList[[k]],
                                  ext   = terra::ext(imageRasterList[[k]]),
                                  scale = scaleImageLayers)
      lyr_name <- names(r)
      outside  <- checkExtent(xs, ys, r)

      vals       <- rep(NA_real_, length(xs))
      inside_idx <- which(!outside)
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


  # New class:
  class(x) <- c("steps_covariates", setdiff(class(x), "steps_covariates"))
  x
}
