#' Simulate an animal track from a step selection process
#'
#' Simulates an animal track with known movement and habitat selection
#' parameters. At every step, a set of candidate steps is drawn from a gamma
#' distribution (step length) and a von Mises distribution (turning angle).
#' Each candidate is scored with a linear predictor built from numeric and/or
#' image raster layers and their selection coefficients, and the next location
#' is drawn among the candidates with softmax (conditional logit)
#' probabilities. Because all parameters are known, simulated tracks can be
#' used to test whether a fitted step selection model recovers them.
#'
#' @param xStart Numeric. x coordinate of the starting location. Must lie
#'   within the spatial overlap of all layers.
#' @param yStart Numeric. y coordinate of the starting location. Must lie
#'   within the spatial overlap of all layers.
#' @param crs `NULL` (default), a numeric EPSG code (e.g. `25832`) or a
#'   character string accepted by [terra::crs()] (e.g. `"EPSG:25832"`). CRS
#'   assigned to the returned track. Must match the CRS of `numericLayers` and
#'   `imageLayers`. If `NULL`, the track is returned without a CRS.
#' @param tStart `POSIXct` of length 1. Timestamp of the starting location.
#' @param timeStep Time between consecutive locations: either a single number
#'   (in seconds) or one of `"sec"`, `"min"`, `"hour"`, `"day"`, `"week"`,
#'   `"month"`, `"quarter"`, `"year"`.
#' @param angleStart Numeric, at least 0 and smaller than 360. Initial heading
#'   in degrees, measured clockwise from north (0 = north, 90 = east).
#' @param nSteps Positive integer. Number of steps to simulate.
#' @param nChoiceSet Positive integer. Number of candidate steps drawn at each
#'   step. Larger values approximate a continuous selection process more
#'   closely but increase computation time.
#' @param scaleSL Positive numeric. Scale parameter of the gamma distribution
#'   for step lengths (in map units). The mean step length of the movement
#'   kernel is `shapeSL * scaleSL`.
#' @param shapeSL Positive numeric. Shape parameter of the gamma distribution
#'   for step lengths.
#' @param kappaTA Non-negative numeric. Concentration parameter of the von
#'   Mises distribution (mean 0) for turning angles. Higher values produce
#'   straighter tracks; `kappaTA = 0` gives uniformly distributed turning
#'   angles.
#' @param numericLayers `NULL` or a `SpatRasterCollection` (see
#'   [terra::sprc()]). Layers evaluated at the end point of each candidate
#'   step (value of a single cell).
#' @param betas Numeric vector with one selection coefficient per layer in
#'   `numericLayers`. Positive values indicate selection, negative values
#'   avoidance. Ignored if `numericLayers = NULL`.
#' @param imageLayers `NULL` or a `SpatRasterCollection`. Layers evaluated
#'   over a square window (kernel) of cells centred on the end point of each
#'   candidate step; the cell values in the window are aggregated to a single
#'   value with the corresponding function in `imageKernelFunctions`.
#' @param imageKernelDimensions Vector of odd positive integers, one per layer
#'   in `imageLayers`. Side length of the kernel window in cells (e.g. `3`
#'   gives a 3 x 3 window).
#' @param imageKernelFunctions List of functions, one per layer in
#'   `imageLayers`. Each function takes a numeric vector (the cell values in
#'   the window) and returns a single number, e.g. `mean` or `max`.
#' @param imageBetas Numeric vector with one selection coefficient per layer in
#'   `imageLayers`. Same interpretation as `betas`.
#' @param scaleNumericLayers Logical. If `TRUE` (default), each numeric layer
#'   is rescaled to the range 0 to 1 using its minimum and maximum within the
#'   shared extent of all layers, so that `betas` are comparable across layers.
#' @param scaleImageLayers Logical. If `TRUE` (default), each image layer is
#'   rescaled to the range 0 to 1 in the same way.
#'
#' @return A `track_xyt` object (see [amt::make_track()]) with columns `x_`,
#'   `y_` and `t_` and `nSteps + 1` rows (the starting location plus one
#'   location per step).
#'
#' @details
#' **Selection process.** For candidate step \eqn{j} with end point
#' \eqn{s_j}, the linear predictor is
#' \deqn{w_j = \sum_k \beta_k z_k(s_j) + \sum_l \gamma_l f_l(K_l(s_j)),}{w_j = sum_k beta_k z_k(s_j) + sum_l gamma_l f_l(K_l(s_j)),}
#' where \eqn{z_k} are the numeric layers with coefficients `betas`,
#' \eqn{K_l(s_j)} is the window of cells of image layer \eqn{l} around
#' \eqn{s_j}, \eqn{f_l} the corresponding function in `imageKernelFunctions`
#' and \eqn{\gamma_l} the coefficients `imageBetas`. The next location is
#' drawn with probability \eqn{\exp(w_j) / \sum_i \exp(w_i)}{exp(w_j) / sum_i exp(w_i)}.
#' Candidate steps represent movement without habitat selection; the step
#' lengths and turning angles of the realised track therefore differ from the
#' gamma and von Mises distributions whenever selection coefficients are not
#' zero.
#'
#' **Boundaries.** All layers are cropped to their shared extent. Candidate
#' end points beyond an edge of this extent are evaluated on a torus, i.e. they
#' take the values from the opposite edge. The coordinates stored in the
#' returned track are *not* wrapped, so the track is a continuous path that can
#' leave the extent of the layers. When covariates are later extracted with
#' [extractStepCovariates()], end points outside the layers are set to `NA`.
#'
#' **Scaling.** Coefficients are only comparable across layers if all layers
#' are on the same scale, i.e. if `scaleNumericLayers` and `scaleImageLayers`
#' are both `TRUE`. Note that [extractStepCovariates()] rescales each layer
#' over its full extent, whereas this function rescales over the shared extent
#' of all layers; use layers with identical extents if covariates extracted
#' afterwards are to be on exactly the same scale as during simulation.
#'
#' **Internal helpers.** Three helper functions, defined at the top of the
#' function body, compute the habitat score of each candidate:
#' - `cropScaleRaster()` crops a layer to the shared extent of all layers and,
#'   if requested, rescales its values to the range 0 to 1.
#' - `extractKernelValues()` reads the square window of cells around each
#'   candidate end point and aggregates it with the kernel function.
#' - `computeImageValues()` applies `extractKernelValues()` to all image
#'   layers, multiplies each result with its coefficient and sums them to one
#'   image score per candidate.
#'
#' A progress message is printed every 1000 steps. Results are random; use
#' [set.seed()] for reproducible tracks.
#'
#' @example inst/examples/simulation-example.R
#'
#' @seealso [createRandomSteps()], [extractStepCovariates()]
#'
#' @author Mika Schubert
#' @export

simulateTrack <- function(xStart                = 0,
                          yStart                = 0,
                          crs                   = NULL,
                          tStart                = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
                          timeStep              = "hour",
                          angleStart            = 0,
                          nSteps                = 100,
                          nChoiceSet            = 100,
                          scaleSL               = 2,
                          shapeSL               = 2,
                          kappaTA               = 2,
                          numericLayers         = NULL,
                          betas                 = c(-3),
                          imageLayers           = NULL,
                          imageKernelDimensions = c(3),
                          imageKernelFunctions  = list(f1 = mean),
                          imageBetas            = c(3),
                          scaleNumericLayers    = TRUE,
                          scaleImageLayers      = TRUE) {

  # Helper function: Crops a raster layer to a given extent and optionally
  # rescales it to (0, 1). Used once per layer, right after the shared overlap
  # extent of all layers has been determined below.
  #   layer - SpatRaster to process
  #   ext   - SpatExtent to crop to (the shared overlap extent, see below)
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
  # Raster borders are wrapped toroidally, consistent with the toroidal
  # boundary conditions used in the simulation loop.
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

  # Helper function: Loops extractKernelValues() over all image layers,
  # multiplies each resulting kernel value with its corresponding beta
  # coefficient and sums everything up. Returns a single "image habitat score"
  # per query point. Returns a vector of zeros if no image layers were
  # supplied, so it can always be added to the numeric-layer contribution
  # without an extra if-check at the call site.
  #   x, y                   - coordinates of the query points
  #   imageRasters           - list of already cropped image SpatRasters
  #   imageBetas             - preference coefficient per image layer
  #   imageKernelDimensions  - kernel side length per image layer
  #   imageKernelFunctions   - aggregation function per image layer
  computeImageValues <- function(x, y,
                                 imageRasters,
                                 imageBetas,
                                 imageKernelDimensions,
                                 imageKernelFunctions) {
    if (length(imageRasters) == 0) return(rep(0, length(x)))
    Reduce("+", lapply(seq_along(imageRasters), function(k) {
      imageBetas[[k]] * extractKernelValues(
        imageRasters[[k]], x, y,
        imageKernelDimensions[k],
        imageKernelFunctions[[k]]
      )
    }))
  }


  # Validation:
  rasterList <- c(terra::as.list(numericLayers), terra::as.list(imageLayers))

  if (!inherits(tStart, "POSIXct") || length(tStart) != 1)
    stop("tStart must be a single value of class 'POSIXct'")
  if (!(is.numeric(timeStep) ||
        timeStep %in% c("sec", "min", "hour", "day", "week", "month", "quarter", "year")) ||
      length(timeStep) != 1)
    stop("timeStep must be a single numeric value or one of: sec, min, hour, day, week, month, quarter, year")
  if (!is.numeric(angleStart) || length(angleStart) != 1 || angleStart >= 360 || angleStart < 0)
    stop("angleStart must be a single positive number between 0 and 360")
  if (!is.numeric(nSteps) || length(nSteps) != 1 || nSteps != floor(nSteps) || nSteps <= 0)
    stop("nSteps must be a single positive integer")
  if (!is.numeric(nChoiceSet) || length(nChoiceSet) != 1 ||
      nChoiceSet != floor(nChoiceSet) || nChoiceSet <= 0)
    stop("nChoiceSet must be a single positive integer")
  if (!is.numeric(scaleSL) || length(scaleSL) != 1 || scaleSL <= 0)
    stop("scaleSL must be a single positive number")
  if (!is.numeric(shapeSL) || length(shapeSL) != 1 || shapeSL <= 0)
    stop("shapeSL must be a single positive number")
  if (!is.numeric(kappaTA) || length(kappaTA) != 1 || kappaTA < 0)
    stop("kappaTA must be a single non-negative number")
  if (length(rasterList) == 0)
    stop("at least one layer must be provided")

  if (length(rasterList) > 1) {
    crsList  <- sapply(rasterList, terra::crs)
    nonEmpty <- crsList[nchar(crsList) > 0]
    if (length(nonEmpty) > 1 && length(unique(nonEmpty)) > 1)
      stop("All layers must have the same CRS")
  }
  if (!is.null(crs)) {
    crs_input <- if (is.numeric(crs)) paste0("EPSG:", crs) else crs
    crs_check <- tryCatch(terra::crs(crs_input), error = function(e) "")
    if (nchar(crs_check) == 0)
      stop("crs must be NULL or a valid coordinate reference system")
    layerCRS <- sapply(rasterList, terra::crs)
    layerCRS <- layerCRS[nchar(layerCRS) > 0]
    if (length(layerCRS) > 0 && !all(sapply(layerCRS, terra::same.crs, crs_input)))
      stop("crs must match the CRS of numericLayers and imageLayers")
  }
  if (!is.null(numericLayers)) {
    if (!inherits(numericLayers, "SpatRasterCollection"))
      stop("numericLayers must be of class 'SpatRasterCollection'")
    if (length(betas) != length(terra::as.list(numericLayers)))
      stop("betas must have one entry per numericLayer")
  }
  if (!is.null(imageLayers)) {
    nImageLayers <- length(terra::as.list(imageLayers))
    if (!inherits(imageLayers, "SpatRasterCollection"))
      stop("imageLayers must be of class 'SpatRasterCollection'")
    if (length(imageKernelDimensions) != nImageLayers)
      stop("imageKernelDimensions must have one entry per imageLayer")
    if (length(imageKernelFunctions) != nImageLayers)
      stop("imageKernelFunctions must have one entry per imageLayer")
    if (length(imageBetas) != nImageLayers)
      stop("imageBetas must have one entry per imageLayer")
    if (any(imageKernelDimensions %% 2 == 0))
      stop("All imageKernelDimensions must be odd numbers")
  }


  # Setting up rasters (cutting all layers to their shared overlap):
  xminAll <- max(sapply(rasterList, function(l) terra::ext(l)[1]))
  xmaxAll <- min(sapply(rasterList, function(l) terra::ext(l)[2]))
  yminAll <- max(sapply(rasterList, function(l) terra::ext(l)[3]))
  ymaxAll <- min(sapply(rasterList, function(l) terra::ext(l)[4]))
  if (xminAll >= xmaxAll || yminAll >= ymaxAll)
    stop("The provided layers must have a spatial overlap")
  extAll  <- terra::ext(xminAll, xmaxAll, yminAll, ymaxAll)
  rasters <- lapply(rasterList, function(l) terra::crop(l, extAll))
  if (xStart < extAll[1] || xStart > extAll[2] || yStart < extAll[3] || yStart > extAll[4])
    stop("Starting coordinates must lie inside the overlapping extent of all layers")

  # Adding image and numeric layers if present (cutting to size and scaling):
  imageRasters <- if (!is.null(imageLayers)) {
    lapply(terra::as.list(imageLayers),
           cropScaleRaster, ext = extAll,
           scale = scaleImageLayers)
  } else list()

  numericRasters <- if (!is.null(numericLayers)) {
    lapply(terra::as.list(numericLayers),
           cropScaleRaster, ext = extAll,
           scale = scaleNumericLayers)
  } else list()

  # Initializing tibble and starting point:
  simData <- tibble::tibble(stepID    = 0:nSteps,
                            x_        = numeric(nSteps + 1),
                            y_        = numeric(nSteps + 1),
                            SL        = numeric(nSteps + 1),
                            Direction = numeric(nSteps + 1),
                            Change    = numeric(nSteps + 1),
                            habitat   = numeric(nSteps + 1))
  simData$x_[1]        <- xStart
  simData$y_[1]        <- yStart
  simData$SL[1]        <- 0
  simData$Direction[1] <- angleStart
  simData$Change[1]    <- 0

  numVal_start <- if (length(numericRasters) == 0) 0 else
    Reduce("+", lapply(seq_along(numericRasters), function(k)
      betas[[k]] * terra::extract(numericRasters[[k]],
                                  cbind(simData$x_[1], simData$y_[1]))[, 1]))

  imgVal_start <- computeImageValues(simData$x_[1],
                                     simData$y_[1],
                                     imageRasters,
                                     imageBetas,
                                     imageKernelDimensions,
                                     imageKernelFunctions)
  simData$habitat[1] <- numVal_start + imgVal_start

  # Simulation loop:
  for (i in 1:nSteps) {
    SLrel     <- stats::rgamma(nChoiceSet, shape = shapeSL, scale = scaleSL)
    ChangeRad <- circular::rvonmises(nChoiceSet, mu = circular::circular(0), kappa = kappaTA)
    Change    <- as.numeric(ChangeRad) * 180 / pi
    TArel     <- simData$Direction[i] + Change
    x         <- simData$x_[i] + SLrel * sin(TArel * pi / 180)
    y         <- simData$y_[i] + SLrel * cos(TArel * pi / 180)
    border    <- terra::ext(rasters[[1]])
    xToro     <- ((x - border[1]) %% (border[2] - border[1])) + border[1]
    yToro     <- ((y - border[3]) %% (border[4] - border[3])) + border[3]
    Direction <- (atan2(x - simData$x_[i], y - simData$y_[i]) * 180 / pi) %% 360

    numLinpred <- if (length(numericRasters) == 0) {
      rep(0, nChoiceSet)
    } else {
      Reduce("+", lapply(seq_along(numericRasters), function(k)
        betas[[k]] * terra::extract(numericRasters[[k]], cbind(xToro, yToro))[, 1]))
    }

    imgLinpred <- computeImageValues(xToro,
                                     yToro,
                                     imageRasters,
                                     imageBetas,
                                     imageKernelDimensions,
                                     imageKernelFunctions)
    linpred    <- numLinpred + imgLinpred

    rawLinpred <- linpred
    linpred    <- linpred - max(linpred)
    p          <- exp(linpred) / sum(exp(linpred))
    multinom   <- stats::rmultinom(1, size = 1, prob = p)
    chosenStep <- which(multinom == 1)

    simData$x_[i + 1]        <- x[chosenStep]
    simData$y_[i + 1]        <- y[chosenStep]
    simData$SL[i + 1]        <- SLrel[chosenStep]
    simData$Direction[i + 1] <- Direction[chosenStep]
    simData$Change[i + 1]    <- Change[chosenStep]
    simData$habitat[i + 1]   <- rawLinpred[chosenStep]

    if (i %% 1000 == 0) message("Step ", i, "/", nSteps)
  }

  # Adding timestamp:
  simData$t_ <- seq(from = tStart, by = timeStep, length.out = nrow(simData))
  x_ = y_ = t_ = NULL

  track <- amt::make_track(simData, x_, y_, t_, crs = if (is.null(crs)) NA else crs)
  return(track)
}
