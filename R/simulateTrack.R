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
#'   over a square window (kernel) of cells around the end point of each
#'   candidate step; the cell values in the window are aggregated to a single
#'   value with the corresponding function in `imageKernelFunctions`.
#' @param imageKernelDimensions Vector of positive integers, one per layer in
#'   `imageLayers`. Side length of the kernel window in cells (e.g. `3` gives
#'   a 3 x 3 window). Odd values give a window centred on the cell that
#'   contains the end point; even values give a window centred on the end
#'   point itself (same cells as [extractStepImagekernels()]).
#' @param imageKernelFunctions List of functions, one per layer in
#'   `imageLayers`. Each function takes the cell values in the window and
#'   returns a single number, e.g. `mean` or `max`. The values are passed as a
#'   numeric vector, or as a matrix if `kernelAsMatrix = TRUE`.
#' @param imageBetas Numeric vector with one selection coefficient per layer in
#'   `imageLayers`. Same interpretation as `betas`.
#' @param scaleNumericLayers Logical. If `TRUE` (default), each numeric layer
#'   is rescaled to the range 0 to 1 using its minimum and maximum within the
#'   shared extent of all layers, so that `betas` are comparable across layers.
#' @param scaleImageLayers Logical. If `TRUE` (default), each image layer is
#'   rescaled to the range 0 to 1 in the same way.
#' @param torus Logical. If `TRUE` (default), the landscape is treated as a
#'   torus: candidate end points and kernel windows beyond an edge of the
#'   layers take the values from the opposite edge. If `FALSE`, the layers are
#'   not wrapped and candidates outside the layers cannot be chosen (see
#'   Details), so the track stays inside the layers.
#' @param kernelAsMatrix Logical. If `FALSE` (default), each function in
#'   `imageKernelFunctions` receives the cell values of the window as a numeric
#'   vector (missing cells removed). If `TRUE`, it receives the complete window
#'   as a matrix with `imageKernelDimensions` rows and columns (row 1 = north,
#'   column 1 = west), so that selection can depend on the spatial arrangement
#'   of the cells within the window, e.g. on the contrast between centre and
#'   surroundings. Windows containing missing cells then give `NA`. Applies to
#'   all image layers.
#' @param returnChoiceSets Logical. If `TRUE`, all candidate steps of all
#'   steps are returned as attribute `"choiceSets"` of the track (see Value).
#'   Default `FALSE`.
#'
#' @return A `track_xyt` object (see [amt::make_track()]) with columns `x_`,
#'   `y_` and `t_` and `nSteps + 1` rows (the starting location plus one
#'   location per step), or fewer rows if the simulation ended early (see
#'   Details). If `returnChoiceSets = TRUE`, the track carries the attribute
#'   `"choiceSets"`, a `data.frame` with one row per candidate step and the
#'   columns `step`, `candidate`, `x`, `y`, one column per numeric layer
#'   (`num_<layer name>`), one column per image layer (`img_<layer name>`,
#'   the aggregated kernel value), `linpred` (linear predictor) and `chosen`
#'   (`TRUE` for the realised step). It can be accessed with
#'   `attr(track, "choiceSets")`.
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
#' **Boundaries.** All layers are cropped to their shared extent. With
#' `torus = TRUE`, candidate end points beyond an edge of this extent are
#' evaluated on a torus, i.e. they take the values from the opposite edge. The
#' coordinates stored in the returned track are *not* wrapped, so the track is
#' a continuous path that can leave the extent of the layers. When covariates
#' are later extracted with [extractStepCovariates()], end points outside the
#' layers are set to `NA`. With `torus = FALSE`, end points and kernel windows
#' are not wrapped; candidates whose end point or kernel window lies (partly)
#' outside the layers get no habitat value.
#'
#' **Missing values.** Candidates without a complete habitat value (`NA` in
#' the linear predictor, e.g. outside the layers with `torus = FALSE` or on
#' `NA` cells) get selection probability 0. If no candidate of a step has a
#' complete habitat value, the simulation stops with a warning and the track
#' is returned up to the last completed step.
#'
#' **Scaling.** Coefficients are only comparable across layers if all layers
#' are on the same scale, i.e. if `scaleNumericLayers` and `scaleImageLayers`
#' are both `TRUE`. Note that [extractStepCovariates()] rescales each layer
#' over its full extent, whereas this function rescales over the shared extent
#' of all layers; use layers with identical extents if covariates extracted
#' afterwards are to be on exactly the same scale as during simulation.
#'
#' **Internal helpers.** Six helper functions, defined at the top of the
#' function body, compute the habitat score of each candidate:
#' - `sameExtent()` checks whether a layer already has the shared extent, so
#'   that it is not cropped unnecessarily.
#' - `cropScaleRaster()` crops a layer to the shared extent of all layers and,
#'   if requested, rescales its values to the range 0 to 1.
#' - `extractKernelValues()` reads the square window of cells around each
#'   candidate end point and aggregates it with the kernel function.
#' - `numericValueMatrix()` and `imageValueMatrix()` return the value of every
#'   numeric layer and the kernel value of every image layer for all
#'   candidates, one column per layer.
#' - `linearPredictor()` multiplies each column with its coefficient and sums
#'   them to one habitat score per candidate.
#'
#' A progress message is printed every 1000 steps. Results are random; use
#' [set.seed()] for reproducible tracks.
#'
#' @example inst/examples/simulation-example.R
#'
#' @seealso [createRandomSteps()], [extractStepCovariates()],
#'   [extractStepImagekernels()]
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
                          scaleImageLayers      = TRUE,
                          torus                 = TRUE,
                          kernelAsMatrix        = FALSE,
                          returnChoiceSets      = FALSE) {

  # Helper function: TRUE if the raster already has exactly the extent 'ext'
  sameExtent <- function(layer, ext) {
    isTRUE(all.equal(as.vector(terra::ext(layer)), as.vector(ext)))
  }

  # Helper function: Crops a raster layer to a given extent and optionally
  # rescales it to (0, 1). Used once per layer, right after the shared overlap
  # extent of all layers has been determined below. A layer that already has
  # exactly this extent is not cropped (cropping a large raster to its own
  # extent only costs time).
  #   layer - SpatRaster to process
  #   ext   - SpatExtent to crop to (the shared overlap extent, see below)
  #   scale - if TRUE, rescale cropped values to (0, 1); skipped if the layer
  #           is constant (min == max), since dividing by zero would occur
  cropScaleRaster <- function(layer, ext, scale = TRUE) {
    if (sameExtent(layer, ext)) {
      r <- layer
    } else {
      r <- terra::crop(layer, ext)
      terra::ext(r) <- ext
    }
    if (scale) {
      r_min <- terra::global(r, "min", na.rm = TRUE)[1, 1]
      r_max <- terra::global(r, "max", na.rm = TRUE)[1, 1]
      if (r_max > r_min) r <- (r - r_min) / (r_max - r_min)
    }
    r
  }

  # Helper function: For each coordinate pair (xs[i], ys[i]), extracts the
  # raster values inside a square window of kernelDim x kernelDim pixels
  # around that point and aggregates them with kernelFun (e.g. mean, max).
  #   - odd kernelDim:  pixel offsets -half, ..., half (window centred on the
  #                     pixel that contains the point)
  #   - even kernelDim: offsets -(kernelDim - 1) / 2, ..., (kernelDim - 1) / 2,
  #                     i.e. half-pixel steps; the window centre is the point
  #                     itself (same pixels as extractStepImagekernels())
  # With torus = TRUE the raster borders are wrapped toroidally, consistent
  # with the toroidal boundary conditions used in the simulation loop; with
  # torus = FALSE points outside the raster give NA.
  #   raster    - SpatRaster to read from
  #   xs, ys    - coordinates of the query points (same length)
  #   kernelDim - side length (in pixels) of the square window
  #   kernelFun - aggregation function applied to the values in each window
  extractKernelValues <- function(raster, xs, ys, kernelDim, kernelFun) {
    res_x <- terra::res(raster)[1]
    res_y <- terra::res(raster)[2]
    steps <- if (kernelDim %% 2 == 1) {
      half <- floor(kernelDim / 2)
      seq(-half, half)
    } else {
      seq(-(kernelDim - 1) / 2, (kernelDim - 1) / 2)
    }

    border  <- terra::ext(raster)
    x_range <- border[2] - border[1]
    y_range <- border[4] - border[3]

    # dx varies fastest: pixel order is west -> east within a row,
    # rows from south to north
    offsets <- expand.grid(dx = steps,
                           dy = steps)
    n_pts <- length(xs)
    n_off <- nrow(offsets)

    all_x <- rep(xs, each = n_off) +
      rep(offsets$dx, times = n_pts) * res_x
    all_y <- rep(ys, each = n_off) +
      rep(offsets$dy, times = n_pts) * res_y

    if (torus) {
      all_x <- ((all_x - border[1]) %% x_range) + border[1]
      all_y <- ((all_y - border[3]) %% y_range) + border[3]
    }

    vals_all <- terra::extract(raster, cbind(all_x, all_y))[, 1]
    vals_mat <- matrix(vals_all, nrow = n_pts,
                       ncol = n_off, byrow = TRUE)

    apply(vals_mat, 1, function(row) {
      if (kernelAsMatrix) {
        # complete window as matrix: row 1 = north, column 1 = west
        if (anyNA(row)) return(NA_real_)
        m <- t(matrix(row, nrow = kernelDim, ncol = kernelDim))[kernelDim:1, , drop = FALSE]
        return(kernelFun(m))
      }
      if (!torus && anyNA(row)) return(NA_real_)   # real landscape: incomplete window is invalid
      v <- row[!is.na(row)]
      if (length(v) == 0) NA_real_ else kernelFun(v)
    })
  }

  # Helper function: value of every numeric layer at the query points,
  # one column per layer (a matrix with 0 columns if there are none)
  numericValueMatrix <- function(x, y) {
    if (length(numericRasters) == 0) return(matrix(numeric(0), nrow = length(x), ncol = 0))
    do.call(cbind, lapply(numericRasters, function(r) terra::extract(r, cbind(x, y))[, 1]))
  }

  # Helper function: kernel value of every image layer at the query points,
  # one column per layer (a matrix with 0 columns if there are none)
  imageValueMatrix <- function(x, y) {
    if (length(imageRasters) == 0) return(matrix(numeric(0), nrow = length(x), ncol = 0))
    do.call(cbind, lapply(seq_along(imageRasters), function(k) {
      extractKernelValues(imageRasters[[k]], x, y,
                          imageKernelDimensions[k],
                          imageKernelFunctions[[k]])
    }))
  }

  # Helper function: multiplies every column with its beta coefficient and
  # sums everything up to one habitat score per query point (zeros if no
  # columns)
  linearPredictor <- function(vals, b) {
    if (ncol(vals) == 0) return(rep(0, nrow(vals)))
    Reduce("+", lapply(seq_len(ncol(vals)), function(k) b[[k]] * vals[, k]))
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
  for (arg in c("torus", "kernelAsMatrix", "returnChoiceSets")) {
    val <- get(arg)
    if (!is.logical(val) || length(val) != 1 || is.na(val))
      stop(arg, " must be a single TRUE or FALSE")
  }
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
    if (any(imageKernelDimensions != floor(imageKernelDimensions)) ||
        any(imageKernelDimensions < 1))
      stop("All imageKernelDimensions must be positive integers")
  }


  # Setting up rasters (cutting all layers to their shared overlap):
  xminAll <- max(sapply(rasterList, function(l) terra::ext(l)[1]))
  xmaxAll <- min(sapply(rasterList, function(l) terra::ext(l)[2]))
  yminAll <- max(sapply(rasterList, function(l) terra::ext(l)[3]))
  ymaxAll <- min(sapply(rasterList, function(l) terra::ext(l)[4]))
  if (xminAll >= xmaxAll || yminAll >= ymaxAll)
    stop("The provided layers must have a spatial overlap")
  extAll  <- terra::ext(xminAll, xmaxAll, yminAll, ymaxAll)
  rasters <- lapply(rasterList, function(l) if (sameExtent(l, extAll)) l else terra::crop(l, extAll))
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

  # Column names for the returned choice sets:
  numNames <- make.unique(vapply(numericRasters, function(r) names(r)[1], character(1)))
  imgNames <- make.unique(vapply(imageRasters,   function(r) names(r)[1], character(1)))

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

  simData$habitat[1] <-
    linearPredictor(numericValueMatrix(xStart, yStart), betas) +
    linearPredictor(imageValueMatrix(xStart, yStart), imageBetas)

  choiceList <- if (returnChoiceSets) vector("list", nSteps) else NULL
  nDone      <- nSteps

  # Simulation loop:
  for (i in 1:nSteps) {
    SLrel     <- stats::rgamma(nChoiceSet, shape = shapeSL, scale = scaleSL)
    ChangeRad <- circular::rvonmises(nChoiceSet, mu = circular::circular(0), kappa = kappaTA)
    Change    <- as.numeric(ChangeRad) * 180 / pi
    TArel     <- simData$Direction[i] + Change
    x         <- simData$x_[i] + SLrel * sin(TArel * pi / 180)
    y         <- simData$y_[i] + SLrel * cos(TArel * pi / 180)
    if (torus) {
      border  <- terra::ext(rasters[[1]])
      xToro   <- ((x - border[1]) %% (border[2] - border[1])) + border[1]
      yToro   <- ((y - border[3]) %% (border[4] - border[3])) + border[3]
    } else {
      xToro   <- x
      yToro   <- y
    }
    Direction <- (atan2(x - simData$x_[i], y - simData$y_[i]) * 180 / pi) %% 360

    numVals    <- numericValueMatrix(xToro, yToro)
    imgVals    <- imageValueMatrix(xToro, yToro)
    rawLinpred <- linearPredictor(numVals, betas) + linearPredictor(imgVals, imageBetas)

    # Candidates with missing habitat values get probability 0:
    valid <- !is.na(rawLinpred)
    if (!any(valid)) {
      warning("Track ended after ", i - 1, " of ", nSteps,
              " steps: no candidate step with complete habitat values.", call. = FALSE)
      nDone <- i - 1
      break
    }
    linpred    <- rawLinpred - max(rawLinpred[valid])
    p          <- exp(linpred)
    p[!valid]  <- 0
    p          <- p / sum(p)
    multinom   <- stats::rmultinom(1, size = 1, prob = p)
    chosenStep <- which(multinom == 1)

    simData$x_[i + 1]        <- x[chosenStep]
    simData$y_[i + 1]        <- y[chosenStep]
    simData$SL[i + 1]        <- SLrel[chosenStep]
    simData$Direction[i + 1] <- Direction[chosenStep]
    simData$Change[i + 1]    <- Change[chosenStep]
    simData$habitat[i + 1]   <- rawLinpred[chosenStep]

    if (returnChoiceSets) {
      cs <- data.frame(step = i, candidate = seq_len(nChoiceSet), x = x, y = y)
      if (ncol(numVals) > 0)
        cs <- cbind(cs, stats::setNames(as.data.frame(numVals), paste0("num_", numNames)))
      if (ncol(imgVals) > 0)
        cs <- cbind(cs, stats::setNames(as.data.frame(imgVals), paste0("img_", imgNames)))
      cs$linpred <- rawLinpred
      cs$chosen  <- seq_len(nChoiceSet) == chosenStep
      choiceList[[i]] <- cs
    }

    if (i %% 1000 == 0) message("Step ", i, "/", nSteps)
  }

  # Removing the unused rows if the track ended early:
  simData <- simData[seq_len(nDone + 1), ]

  # Adding timestamp:
  simData$t_ <- seq(from = tStart, by = timeStep, length.out = nrow(simData))
  x_ = y_ = t_ = NULL

  track <- amt::make_track(simData, x_, y_, t_, crs = if (is.null(crs)) NA else crs)
  if (returnChoiceSets) attr(track, "choiceSets") <- do.call(rbind, choiceList[seq_len(nDone)])
  return(track)
}
