#' Extract image kernels around step end points
#'
#' For each observed and random step, cuts out a square image section (image
#' kernel) centred on the step end point (`x2_`, `y2_`) from a set of
#' georeferenced GeoTIFF files, e.g. orthophoto tiles. Each kernel is stored as
#' a three-dimensional array (rows x columns x bands) with values rescaled to
#' the range 0 to 1. The kernels are the input for [cnn_ssf()].
#'
#' @param x An object of class `random_steps_prop`, as returned by
#'   [createRandomSteps()], or of class `steps_covariates`, as returned by
#'   [extractStepCovariates()] or a previous call of this function.
#' @param tifDirectory Character. Path to a folder containing one or several
#'   GeoTIFF files (extension `.tif`) that together cover the study area. All
#'   files must share CRS, resolution and number of bands. They are combined
#'   into a virtual mosaic with [terra::vrt()], without loading them into
#'   memory.
#' @param kernelRadius Positive numeric. Half the side length of the square
#'   kernel, in map units of the raster CRS (metres for projected CRS such as
#'   EPSG:25832). The kernel covers the end point plus and minus
#'   `kernelRadius` in x and y direction.
#' @param imageScaling `NULL` (default) or a numeric vector `c(min, max)` used
#'   to rescale all bands as `(value - min) / (max - min)`, e.g. `c(0, 255)`
#'   for 8-bit images. If `NULL`, each band is rescaled with its minimum and
#'   maximum over the whole mosaic, which requires reading all files and can be
#'   slow for large mosaics.
#' @param colName `NULL` (default) or a single character string. Name of the
#'   new column. If `NULL`, the name of the folder `tifDirectory` is used.
#'
#' @return An object of class `steps_covariates` (in addition to the classes of
#'   `x`) with one additional list column named `colName`. Each element is a
#'   numeric array with dimensions `c(rows, columns, bands)`, as returned by
#'   [terra::as.array()]: the first row is the northern edge and the first
#'   column the western edge of the kernel. Elements are `NA` for steps whose
#'   end point lies outside the mosaic (with a warning).
#'
#' @details
#' **Kernel size.** A kernel spans about `2 * kernelRadius / res` cells per
#' side, where `res` is the cell size of the images; its borders are snapped to
#' the nearest cell borders. Choose `kernelRadius` such that
#' `2 * kernelRadius` is a multiple of the cell size to obtain kernels of
#' identical size, which [cnn_ssf()] requires. Kernels at the edge of the
#' mosaic are truncated and therefore smaller; gaps between tiles appear as
#' `NA` cells. Kernel dimensions can be checked with
#' `table(sapply(x[[colName]], function(a) paste(dim(a), collapse = " x ")))`.
#'
#' **Input for [cnn_ssf()].** [cnn_ssf()] expects an array with dimensions
#' `c(steps, bands, rows, columns)`. If all kernels have the same size and no
#' element is `NA`, it can be built with
#' `aperm(simplify2array(x[[colName]]), c(4, 3, 1, 2))`.
#'
#' **Memory.** Kernels are stored as double-precision arrays. For example, a
#' kernel of 100 x 100 cells with 4 bands needs about 0.3 MB, so 10 000 steps
#' need about 3 GB of memory.
#'
#' **CRS.** The coordinates of `x` are assumed to be in the CRS of the images;
#' a warning is issued if the images have no CRS.
#'
#' @example inst/examples/dataprep-example.R
#'
#' @seealso [createRandomSteps()], [extractStepCovariates()], [cnn_ssf()]
#'
#' @author Mika Schubert
#' @export

extractStepImagekernels <- function(x,
                                    tifDirectory = NULL,
                                    kernelRadius = NULL,
                                    imageScaling = NULL,
                                    colName      = NULL) {

  # Validation:
  if (!inherits(x, c("random_steps_prop", "steps_covariates")))
    stop("`x` must be an object of class 'random_steps_prop' or 'steps_covariates'.")
  if (is.null(tifDirectory) || !is.character(tifDirectory) || length(tifDirectory) != 1)
    stop("`tifDirectory` must be a single character string.")
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

  # Building virtual mosaic out of all .tif files with terra::vrt():
  tif_files <- list.files(tifDirectory, pattern = "\\.tif$", full.names = TRUE)
  if (length(tif_files) == 0) stop("No .tif files in: ", tifDirectory)

  vrt_path <- tempfile(fileext = ".vrt")
  r        <- terra::vrt(tif_files, filename = vrt_path, overwrite = TRUE)
  n_bands  <- terra::nlyr(r)

  # Column name: user-provided or derived from folder name
  if (is.null(colName)) {
    colName <- basename(normalizePath(tifDirectory, mustWork = FALSE))
  }

  # CRS check:
  if (is.na(terra::crs(r)) || terra::crs(r) == "")
    warning("Raster has no defined CRS. Make sure coordinates and images share the same projection.")

  # Coordinates as SpatVector:
  pts <- terra::vect(as.data.frame(x), geom = c("x2_", "y2_"), crs = terra::crs(r))

  # Check if steps lie outside the raster coverage:
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

  # Determine band min/max for normalization:
  if (!is.null(imageScaling)) {
    band_min <- rep(imageScaling[1], n_bands)
    band_max <- rep(imageScaling[2], n_bands)
  } else {
    band_range <- terra::global(r, fun = "range", na.rm = TRUE)
    band_min   <- band_range[, "min"]
    band_max   <- band_range[, "max"]
  }

  # Preparing list for image kernels (all elements start as NULL):
  kernel_list <- vector("list", nrow(x))

  for (i in seq_len(nrow(x))) {

    if (outside[i]) next

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
    } else {
      kernel_list[[i]] <- kernel
    }
  }

  # Adding kernels as one list column with 3D arrays [nrow, ncol, n_bands]:
  x[[colName]] <- lapply(kernel_list, function(k) {
    if (is.null(k)) return(NA)

    nr       <- terra::nrow(k)
    nc       <- terra::ncol(k)
    combined <- array(NA_real_, dim = c(nr, nc, n_bands))

    arr_3d <- terra::as.array(k)
    for (b_idx in seq_len(n_bands)) {
      combined[, , b_idx] <- (arr_3d[, , b_idx] - band_min[b_idx]) /
        (band_max[b_idx] - band_min[b_idx])
    }

    combined
  })

  class(x) <- c("steps_covariates", setdiff(class(x), "steps_covariates"))
  return(x)
}
