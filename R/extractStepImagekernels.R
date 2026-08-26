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
#'  (one or multiple .tif)
#' @param kernelRadius Radius of the kernel around the step coordinates in meters
#' @param imageScaling Possibility to set known scale (e.g. c(0,255) for INT1U),
#'  if no scale provided, image data will be scaled with global min max.
#' @param colName name of the column of kernels in the data frame
#'
#' @return An object of class \code{steps_covariates} (and all classes inherited
#'   from \code{x}) with one additional column per image band added.
#'   Column names match the names of each image band.
#'
#' @details
#' All image and coordinate data must share the same CRS.
#'
#' @seealso \code{\link{createRandomSteps}}
#'
#'
#' @example inst/examples/dataprep-example.R
#'
#' @export
#' @author Mika Schubert


extractStepImagekernels <- function(x,
                                    tifDirectory = NULL,
                                    kernelRadius = NULL,
                                    imageScaling = NULL,
                                    colName      = NULL) {

  # Validation

  if (!inherits(x, c("random_steps_prop", "steps_covariates")))
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
  if (!is.null(colName) && (!is.character(colName) || length(colName) != 1))
    stop("`colName` must be a single character string.")

  # Building virtual mosaic out of all .tif data with terra::vrt()

  tif_files <- list.files(tifDirectory, pattern = "\\.tif$", full.names = TRUE)
  if (length(tif_files) == 0) stop("No .tif files in: ", tifDirectory)

  vrt_path <- tempfile(fileext = ".vrt")
  r        <- terra::vrt(tif_files, filename = vrt_path, overwrite = TRUE)
  n_bands  <- terra::nlyr(r)

  # Column name: user-provided or derived from folder name
  if (is.null(colName)) {
    colName <- basename(normalizePath(tifDirectory, mustWork = FALSE))
  }

  # CRS check
  if (is.na(terra::crs(r)) || terra::crs(r) == "")
    warning("Raster has no defined CRS. Make sure coordinates and images share the same projection.")

  # Coordinates as SpatVector
  pts <- terra::vect(as.data.frame(x), geom = c("x2_", "y2_"), crs = terra::crs(r))

  # Check if steps lie outside the raster coverage
  mosaic_ext <- terra::ext(r)
  xs <- x$x2_
  ys <- x$y2_

  outside <- xs < mosaic_ext[1] | xs > mosaic_ext[2] |
    ys < mosaic_ext[3] | ys > mosaic_ext[4]

  if (any(outside)) {
    warning(
      "Step(s) ", paste(which(outside), collapse = ", "),
      " lie outside the raster coverage; image band values set to NA.",
      call. = FALSE
    )
  }

  # Determine band min/max for normalization
  if (!is.null(imageScaling)) {
    band_min <- rep(imageScaling[1], n_bands)
    band_max <- rep(imageScaling[2], n_bands)
  } else {
    band_range <- terra::global(r, fun = "range", na.rm = TRUE)
    band_min   <- band_range[, "min"]
    band_max   <- band_range[, "max"]
  }

  # Preparing list for image kernels
  kernel_list <- vector("list", nrow(x))

  for (i in seq_len(nrow(x))) {

    if (outside[i]) {
      kernel_list[[i]] <- list(NULL)
      next
    }

    pt <- pts[i, ]
    e  <- terra::ext(
      terra::crds(pt)[1] - kernelRadius, terra::crds(pt)[1] + kernelRadius,
      terra::crds(pt)[2] - kernelRadius, terra::crds(pt)[2] + kernelRadius
    )

    kernel <- terra::crop(r, e, snap = "near")

    if (terra::nrow(kernel) == 0 || terra::ncol(kernel) == 0 ||
        terra::nlyr(kernel) != n_bands) {
      warning("Step ", i, ": kernel could not be extracted; values set to NA.",
              call. = FALSE)
      kernel_list[[i]] <- list(NULL)
    } else {
      kernel_list[[i]] <- kernel
    }
  }

  # Adding kernels as one column with 3D array [nrow, ncol, n_bands]
  x[[colName]] <- lapply(kernel_list, function(k) {
    if (is.null(k)) return(NA)

    nr       <- terra::nrow(k)
    nc       <- terra::ncol(k)
    combined <- array(NA_real_, dim = c(nr, nc, n_bands))

    arr_3d <- terra::as.array(k)                    # <-- einmal, für alle Bänder
    for (b_idx in seq_len(n_bands)) {
      combined[, , b_idx] <- (arr_3d[, , b_idx] - band_min[b_idx]) /
        (band_max[b_idx] - band_min[b_idx])
    }

    combined
  })

  class(x) <- c("steps_covariates", class(x))
  return(x)
}
