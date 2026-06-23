
#' Create Random Steps for Step Selection Functions
#'
#' Adds random (control) steps to a track or steps object, using either
#' a 2D kernel density estimator (via \pkg{ks}) fitted on step length
#' and turning angle, or the traditional approach (gamma + von Mises distributions).
#'
#' @param x An \code{amt} object of class \code{steps_xy} or \code{track_xy}.
#' @param estimator character(1). Either \code{"density"} (2D KDE via \pkg{ks})
#'   or \code{"traditional"} (gamma step-length + von Mises turning-angle).
#'   Default: "density".
#' @param n_control positive integer. Number of random steps per observed step.
#'   Default: 10.
#'
#' @return When \code{estimator = "density"}, an object of class \code{"random_steps_kde"}
#' with \code{n_control} random alternatives per observed step.
#' When \code{estimator = "traditional"}, an object of class \code{"random_steps"}.
#'
#'
#' @details
#' When \code{estimator = "density"}, step lengths (SL) and turning
#' angles (TA, in radians) are extracted from all observed steps, a
#' bivariate KDE is fitted and random steps are drawn.
#' When \code{estimator = "traditional"}, the function \code{\link[amt]{random_steps}}
#' of the amt package is used.
#'
#'
#' @example inst/examples/dataprep-example.R
#'
#' @export
#' @author Mika Schubert


createRandomSteps <- function(x,
                              estimator  = c("density", "traditional"),
                              n_control  = 10) {
  # Validation
  if (!inherits(x, c("steps_xy", "steps_xyt", "track_xy", "track_xyt"))) {
    stop("`x` must be an amt `steps_xy`, `steps_xyt`, `track_xy`, or `track_xyt` object.")}
  if (!is.numeric(n_control) || length(n_control) != 1L ||
      n_control != floor(n_control) || n_control < 1L) {
    stop("`n_control` must be a single positive integer.")}
  n_control <- as.integer(n_control)

  # If class track transform to class steps
  if (inherits(x, c("track_xy", "track_xyt")) &&
      !inherits(x, c("steps_xy", "steps_xyt"))) {
    x <- amt::steps(x)}
  x$step_id_ <- seq_len(nrow(x))
  estimator <- match.arg(estimator)

  # If traditional estimator use amt function random_steps
  if (estimator == "traditional") {
    valid   <- !is.na(x$ta_) & !is.na(x$direction_p) & !is.na(x$sl_)
    x       <- x[valid, ]
    out     <- amt::random_steps(x, n_control = n_control)
    sl_fit  <- attr(out, "sl_")
    ta_fit  <- attr(out, "ta_")
    sl_prob <- stats::dgamma(out$sl_,
                             shape = sl_fit$params$shape,
                             scale = sl_fit$params$scale)
    ta_prob <- as.numeric(circular::dvonmises(
      circular::circular(out$ta_),
      mu    = ta_fit$params$mu,
      kappa = ta_fit$params$kappa))
    out$probability <- sl_prob * ta_prob
    class(out) <- c("random_steps_prop", class(out))
    return(out)}

  # Drop incomplete steps (e.g. first step  with missing ta_)
  valid <- !is.na(x$ta_) & !is.na(x$direction_p) & !is.na(x$sl_)
  x_valid <- x[valid, ]

  obs_movement <- cbind(sl = x_valid$sl_, ta = x_valid$ta_)

  # Fit bivariate KDE
  sl_max <- max(x_valid$sl_) * 2
  H_hat   <- ks::Hpi(obs_movement)
  kde_fit <- ks::kde(obs_movement, H = H_hat, xmin = c(0, -pi),xmax = c(sl_max,  pi))

  # Helper function: draw n sl-ta-pairs
  draw_kde <- function(n, kde_obj) {
    candidates           <- ks::rkde(n * 2, fhat = kde_obj)
    keep                 <- candidates[candidates[, "sl"] > 0 &
                                         candidates[, "ta"] >= -pi &
                                         candidates[, "ta"] <= pi, , drop = FALSE]
    if (nrow(keep) < n) stop("Too many drawn samples out of range.")
    keep[seq_len(n), ]
  }

  # Draw sl-ta-pairs for all valid steps
  n_obs        <- nrow(x_valid)
  n_total      <- n_obs * n_control
  random_draws <- draw_kde(n_total, kde_fit)
  sl_random      <- random_draws[, "sl"]
  ta_random      <- random_draws[, "ta"]

  # Reconstruct end coordinates
  obs_idx   <- rep(seq_len(n_obs), each = n_control)
  abs_angle <- x_valid$direction_p[obs_idx] + ta_random
  x2_random   <- x_valid$x1_[obs_idx] + sl_random * cos(abs_angle)
  y2_random   <- x_valid$y1_[obs_idx] + sl_random * sin(abs_angle)

  # Build random-step rows
  random_tbl          <- x_valid[obs_idx, ]
  random_tbl$x2_      <- x2_random
  random_tbl$y2_      <- y2_random
  random_tbl$sl_      <- sl_random
  random_tbl$ta_      <- ta_random
  random_tbl$case_    <- FALSE
  random_tbl$step_id_ <- rep(x_valid$step_id_, each = n_control)
  random_tbl$probability <- stats::predict(kde_fit,
                                           x = cbind(sl_random, ta_random))
  # Observed rows
  obs_tbl        <- x_valid
  obs_tbl$case_  <- TRUE
  obs_tbl$probability <- stats::predict(kde_fit,
                                        x = cbind(x_valid$sl_, x_valid$ta_))

  # Bind, sort, and select columns
  core_cols <- c("step_id_", "case_", "probability", "x1_", "x2_", "y1_", "y2_", "sl_", "ta_")
  time_cols <- intersect(c("t1_", "t2_", "dt_"), names(x_valid))
  keep_cols <- c(core_cols, time_cols)

  out <- rbind(obs_tbl[, keep_cols], random_tbl[, keep_cols])
  out <- out[order(out$step_id_, !out$case_), ]

  # Set class
  base_steps_class <- if (inherits(x, "steps_xyt")) "steps_xyt" else "steps_xy"
  class(out) <- c(
    "random_steps_prop",
    "random_steps",
    base_steps_class,
    setdiff(class(out), c("random_steps_prop", "random_steps",
                          "steps_xyt", "steps_xy"))
  )
  out
}



#' Extract Habitat Covariates for Random Steps
#'
#' Extracts covariate values at the coordinates of each step in a
#' \code{random_steps_prop} object. For numeric layers, values are extracted
#' directly at the endpoint. For image layers, kernel-aggregated values are
#' computed via \code{\link{extractKernelValues}}. Column names are taken from
#' the layer names of each \code{SpatRaster}. Coordinates that lie
#' outside a layer's extent are set to \code{NA} and a warning is issued.
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
#' @seealso \code{\link{createRandomSteps}}, \code{\link{extractKernelValues}}
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


  # Helper function: warning if coordinates out of extend

  checkExtent <- function(xs, ys, raster) {
    ext      <- terra::ext(raster)
    lyr_name <- names(raster)
    outside  <- xs < ext[1] | xs > ext[2] | ys < ext[3] | ys > ext[4]
    if (any(outside, na.rm = TRUE))
      warning(sum(outside, na.rm = TRUE),
              " coordinate(s) out of layer '", lyr_name, "' extent; set to NA.",
              call. = FALSE)
    outside}

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




#' Extract Image kernels around Random Steps for CNN analysis
#'
#' Extracts image kernels around the coordinates of each step in a
#' \code{random_steps_prop} object. Images must be provided as one ore
#' multiple .tif images with the same projection as the random step
#' coordinates. Column names are taken from the image band names.
#'
#' @param x An object of class \code{random_steps_prop} as returned by
#'   \code{\link{createRandomSteps}}
#' @param tifDirectory Directory to folder containing the .tif data
#' @param kernelRadius Radius of the kernel around the step coordinates in meters
#' @param imageScaling Possibility to set known scale (e.g. c(0,255) for INT1U),
#'  if no scale provided, image data will be scaled with global min max.
#'
#' @return An object of class \code{steps_covariates} (and all classes inherited
#'   from \code{x}) with one additional column per image band added.
#'   Column names match the names of each image band.
#'
#' @details
#' All image and coordinate data must share the same CRS.
#'
#' @seealso \code{\link{createRandomSteps}}, \code{\link{extractKernelValues}}
#'
#'
#' @example inst/examples/dataprep-example.R
#'
#' @export
#' @author Mika Schubert


extractStepImagekernels <- function(x,
                                    tifDirectory = NULL,
                                    kernelRadius = NULL,
                                    imageScaling = NULL) {

  # Validation

  if (!inherits(x, c("random_steps_prop","steps_covariates") ))
    stop("`x` must be an object of class 'random_steps_prop' or 'steps_covariates'.")
  if (!dir.exists(tifDirectory))
    stop("Directory does not exist: ", tifDirectory)
  if (is.null(kernelRadius))
    stop("`kernelRadius` must be provided.")
  if (!is.numeric(kernelRadius) || length(kernelRadius) != 1)
    stop("`kernelRadius` must be a single numeric value.")
  if (kernelRadius <= 0)
    stop("`kernelRadius` must be a positive number.")
  if (!is.null(imageScaling)) {
    if (!is.numeric(imageScaling) || length(imageScaling) != 2)
      stop("`imageScaling` must be a numeric vector of length 2, e.g. c(0, 255).")
    if (imageScaling[1] >= imageScaling[2])
      stop("`imageScaling[1]` must be smaller than `imageScaling[2]`.")
  }


  # Building virtual mosaic out of all .tif data with terra::vrt()

  tif_files <- list.files(tifDirectory, pattern = "\\.tif$", full.names = TRUE)
  if (length(tif_files) == 0) stop("No .tif files in: ", tifDirectory)

  vrt_path <- tempfile(fileext = ".vrt")
  r <- terra::vrt(tif_files, filename = vrt_path, overwrite = TRUE)

  # Band names
  folder_name <- basename(normalizePath(tifDirectory, mustWork = FALSE))
  band_names  <- paste0(folder_name, "_band", seq_len(terra::nlyr(r)))
  names(r)    <- band_names

  # CRS check
  if (is.na(terra::crs(r)) || terra::crs(r) == "")
    warning("Raster has no defined CRS. Make sure coordinates and images share the same projection.")

  # Coordinates as SpatVector
  pts <- terra::vect(as.data.frame(x), geom = c("x2_", "y2_"), crs = terra::crs(r))

  # --- Check which steps lie outside the raster coverage ---

  mosaic_ext <- terra::ext(r)
  xs <- x$x2_
  ys <- x$y2_

  outside <- xs < mosaic_ext[1] | xs > mosaic_ext[2] |
    ys < mosaic_ext[3] | ys > mosaic_ext[4]

  if (any(outside)) {
    bad_steps <- which(outside)
    warning(
      "Step(s) ", paste(bad_steps, collapse = ", "),
      " lie outside the raster coverage; image band values set to NA.",
      call. = FALSE
    )
  }

  # --- Determine band min/max for normalization ---

  if (!is.null(imageScaling)) {
    # Use user-provided fixed scaling range for all bands
    band_min <- rep(imageScaling[1], length(band_names))
    band_max <- rep(imageScaling[2], length(band_names))
  } else {
    # Compute global min/max per band from the raster mosaic
    band_range <- terra::global(r, fun = "range", na.rm = TRUE)
    band_min <- band_range[, "min"]
    band_max <- band_range[, "max"]
  }

  # preparing list for image kernels
  kernel_list <- vector("list", nrow(x))

  for (i in seq_len(nrow(x))) {

    # Skip steps outside raster coverage -> NA kernel
    if (outside[i]) {
      kernel_list[[i]] <- NULL
      next
    }

    pt <- pts[i, ]

    # setting radius
    e <- terra::ext(
      terra::crds(pt)[1] - kernelRadius, terra::crds(pt)[1] + kernelRadius,
      terra::crds(pt)[2] - kernelRadius, terra::crds(pt)[2] + kernelRadius
    )

    # cropping kernels out of necessary .tifs
    kernel <- terra::crop(r, e, snap = "near")

    # additional safety check: empty or inconsistent kernel
    if (terra::nrow(kernel) == 0 || terra::ncol(kernel) == 0 ||
        terra::nlyr(kernel) != length(band_names)) {
      warning("Step ", i, ": kernel could not be extracted (out of coverage); ",
              "values set to NA.", call. = FALSE)
      kernel_list[[i]] <- NULL
    } else {
      kernel_list[[i]] <- kernel
    }
  }

  # adding kernels to data frame (with consistent normalization across all kernels)
  for (b_idx in seq_along(band_names)) {
    b <- band_names[b_idx]
    b_min <- band_min[b_idx]
    b_max <- band_max[b_idx]

    x[[b]] <- lapply(kernel_list, function(k) {
      if (is.null(k)) return(NA)

      arr <- terra::as.array(k[[b]])
      (arr - b_min) / (b_max - b_min)
    })
  }

  class(x) <- c("steps_covariates", class(x))

  return(x)
}
