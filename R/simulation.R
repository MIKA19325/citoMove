
#' @title Simulate Animal Tracks
#'
#' @description Generates a simulated animal track using the Step Selection Function (SSF)
#' method. At each step, a set of possible steps is drawn from movement distributions
#' (gamma for step length, von Mises for turning angle). Each possible step is evaluated
#' using preference score derived from numeric and/or image raster layers of habitat variables,
#' weighted by their respective beta coefficients. The next location is then chosen
#' via a softmax (conditional logit) draw over the candidate steps, where selection
#' probability increases exponentially with habitat score.
#'
#' @param xStart numeric. Starting x coordinate of the animal. Must lie within the
#'   spatial overlap of all provided layers. Default: 0.
#' @param yStart numeric. Starting y coordinate of the animal. Must lie within the
#'   spatial overlap of all provided layers. Default: 0.
#' @param tStart POSIXct. Starting timestamp of the track. Default: 2026-01-01 00:00:00 UTC.
#' @param timeStep character or numeric. Time elapsed per step. Either a numeric value
#'   (seconds) or one of: "sec", "min", "hour", "day", "week", "month", "quarter", "year".
#'   Default: "hour".
#' @param angleStart numeric. Starting direction of movement in degrees clockwise from
#'   due north. Must be in (0, 360). Default: 0.
#' @param nSteps positive integer. Number of steps to simulate. Default: 100.
#' @param nChoiceSet positive integer. Number of candidate steps drawn at each step
#'   (i.e. size of the choice set). Larger values increase accuracy but slow simulation.
#'   Default: 100.
#' @param scaleSL positive numeric. Scale parameter of the gamma distribution used to
#'   draw step lengths. Default: 2.
#' @param shapeSL positive numeric. Shape parameter of the gamma distribution used to
#'   draw step lengths. Default: 2.
#' @param kappaTA non-negative numeric. Concentration parameter of the von Mises
#'   distribution used to draw turning angles. Higher values produce straighter tracks.
#'   kappa = 0 corresponds to uniform (random) turning. Default: 2.
#' @param numericLayers SpatRasterCollection. Continuous or binary raster layers used
#'   as habitat covariates. Must have the same CRS as all other layers. If
#'   \code{scaleNumericLayers = TRUE}, each layer is scaled to (0, 1) internally.
#' @param betas numeric vector. Habitat preference coefficients for each layer in
#'   \code{numericLayers}. Must have the same length as \code{numericLayers}. Positive
#'   values indicate preference, negative values indicate avoidance.
#' @param imageLayers SpatRasterCollection. Raster layers treated as image-like
#'   covariates, evaluated via a spatial kernel rather than at a single point. Must
#'   have the same CRS as all other layers. If \code{scaleImageLayers = TRUE}, each
#'   layer is scaled to (0, 1) internally.
#' @param imageKernelDimensions odd positive integer vector. Spatial kernel size (in
#'   pixels) for each image layer. Must be odd and have one entry per image layer.
#'   Default: c(3).
#' @param imageKernelFunctions list of functions. Aggregation function applied to each
#'   kernel window (e.g. \code{mean}, \code{max}). Must have one entry per image layer.
#'   Default: list(f1 = mean).
#' @param imageBetas numeric vector. Habitat preference coefficients for each image
#'   layer. Same interpretation as \code{betas}. Default: c(3).
#' @param scaleNumericLayers logical. If TRUE, each numeric layer is scaled to (0, 1)
#'   based on its observed min/max before simulation. This makes \code{betas} comparable
#'   across layers with different value ranges. Default: TRUE.
#' @param scaleImageLayers logical. If TRUE, each image layer is scaled to (0, 1)
#'   based on its observed min/max before simulation. Default: TRUE.
#'
#' @return An \code{amt} track object (class \code{track_xy}) with columns
#'   \code{x_}, \code{y_}, and \code{t_}.
#'
#' @details
#' All layers are cropped to their spatial overlap before simulation. Movement uses
#' toroidal boundary conditions, meaning the layers repeat indefinitely at the edges of the
#' landscape. Beta coefficients are only comparable across layers if all layers are
#' on the same scale (i. e. \code{scaleNumericLayers} and \code{scaleImageLayers} are both TRUE).
#'
#'
#' @examples
#' \dontrun{
#' # see system.file("examples/simulation-example.R", package = "citoMove")
#' }
#'
#' @export
#' @author Mika Schubert


simulateTrack <- function(xStart = 0,
                          yStart = 0,
                          tStart = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
                          timeStep = "hour",
                          angleStart = 0,
                          nSteps = 100,
                          nChoiceSet = 100,
                          scaleSL = 2,
                          shapeSL = 2,
                          kappaTA = 2,
                          numericLayers = NULL,
                          betas = c(-3),
                          imageLayers = NULL,
                          imageKernelDimensions = c(3),
                          imageKernelFunctions = list(f1 = mean),
                          imageBetas = c(3),
                          scaleNumericLayers = TRUE,
                          scaleImageLayers = TRUE)
{

# Validation:
  rasterList <- c(terra::as.list(numericLayers), terra::as.list(imageLayers))
  if (!inherits(tStart, "POSIXct") || length(tStart) != 1) stop("tStart must be a single value of class 'POSIXct'")
  if (!(is.numeric(timeStep) || timeStep %in% c("sec", "min", "hour", "day", "week", "month", "quarter", "year")) || length(timeStep) != 1)
    stop("timeStep must be a single numeric value or one of: sec, min, hour, day, week, month, quarter, year")
  if (!is.numeric(angleStart) || length(angleStart) != 1 || angleStart >= 360 || angleStart < 0) stop("angleStart must be a single positive number between 0 and 360")
  if (!is.numeric(nSteps) || length(nSteps) != 1 || nSteps != floor(nSteps) || nSteps <= 0) stop("nSteps must be a single positive integer")
  if (!is.numeric(nChoiceSet) || length(nChoiceSet) != 1 || nChoiceSet != floor(nChoiceSet) || nChoiceSet <= 0) stop("nChoiceSet must be a single positive integer")
  if (!is.numeric(scaleSL) || length(scaleSL) != 1 || scaleSL <= 0) stop("scaleSL must be a single positive number")
  if (!is.numeric(shapeSL) || length(shapeSL) != 1 || shapeSL <= 0) stop("shapeSL must be a single positive number")
  if (!is.numeric(kappaTA) || length(kappaTA) != 1 || kappaTA < 0) stop("kappaTA must be a single non-negative number")
  if (length(rasterList) == 0) stop("at least one layer must be provided")
  if (length(rasterList) > 1) {
    if (!all(sapply(rasterList[-1], function(l) terra::crs(l) == terra::crs(rasterList[[1]]))))
      stop("All layers must have the same CRS")}
  if (!is.null(numericLayers)) {
    if (!inherits(numericLayers, "SpatRasterCollection")) stop("numericLayers must be of class 'SpatRasterCollection'")
    if (length(betas) != length(terra::as.list(numericLayers))) stop("betas must have one entry per numericLayer")
  }
  if (!is.null(imageLayers)) {
    nImageLayers <- length(terra::as.list(imageLayers))
    if (!inherits(imageLayers, "SpatRasterCollection")) stop("imageLayers must be of class 'SpatRasterCollection'")
    if (length(imageKernelDimensions) != nImageLayers) stop("imageKernelDimensions must have one entry per imageLayer")
    if (length(imageKernelFunctions) != nImageLayers) stop("imageKernelFunctions must have one entry per imageLayer")
    if (length(imageBetas) != nImageLayers) stop("imageBetas must have one entry per imageLayer")
    if (any(imageKernelDimensions %% 2 == 0)) stop("All imageKernelDimensions must be odd numbers")
  }


# Setting up numericLayer Rasters: (cutting to overlap size)
  xminAll <- max(sapply(rasterList, function(l) terra::ext(l)[1]))
  xmaxAll <- min(sapply(rasterList, function(l) terra::ext(l)[2]))
  yminAll <- max(sapply(rasterList, function(l) terra::ext(l)[3]))
  ymaxAll <- min(sapply(rasterList, function(l) terra::ext(l)[4]))
  extAll  <- terra::ext(xminAll, xmaxAll, yminAll, ymaxAll)
  rasters <- lapply(rasterList, function(l) terra::crop(l, extAll))
  if (xStart < extAll[1] || xStart > extAll[2] || yStart < extAll[3] || yStart > extAll[4]) stop("Starting coordinates must be inside of overlaping layer extend")
  if (xminAll >= xmaxAll || yminAll >= ymaxAll) stop("The provided layers must have a spatial overlap")

# Adding imageLayer if present: (cutting to size and scaling)
  if (!is.null(imageLayers)) {
    imageRasterList <- terra::as.list(imageLayers)
    imageRasters <- lapply(imageRasterList, function(l) {
      r <- terra::crop(l, extAll)
      if (scaleImageLayers) {
        r_min <- terra::global(r, "min", na.rm = TRUE)[1, 1]
        r_max <- terra::global(r, "max", na.rm = TRUE)[1, 1]
        if (r_max > r_min) r <- (r - r_min) / (r_max - r_min)}
      r})
  } else {imageRasters <- list()}


# Adding numericLayer if present: (cutting to size and scaling)
  if (!is.null(numericLayers)) {
    numericRasterList <- terra::as.list(numericLayers)
    numericRasters <- lapply(numericRasterList, function(l) {
      r <- terra::crop(l, extAll)
      if (scaleNumericLayers) {
        r_min <- terra::global(r, "min", na.rm = TRUE)[1, 1]
        r_max <- terra::global(r, "max", na.rm = TRUE)[1, 1]
        if (r_max > r_min) r <- (r - r_min) / (r_max - r_min)}
      r})
  } else {numericRasters <- list()}

# Kernel extraction helper function:
  extractKernelValues_vec <- function(raster, xs, ys, kernelDim, kernelFun) {
    res_x <- terra::res(raster)[1]
    res_y <- terra::res(raster)[2]
    half  <- floor(kernelDim / 2)

    offsets <- expand.grid(dx = seq(-half, half), dy = seq(-half, half))
    n_pts    <- length(xs)
    n_off    <- nrow(offsets)

    all_x <- rep(xs, each = n_off) + rep(offsets$dx, times = n_pts) * res_x
    all_y <- rep(ys, each = n_off) + rep(offsets$dy, times = n_pts) * res_y

    vals_all <- terra::extract(raster, cbind(all_x, all_y))[, 1]

    vals_mat <- matrix(vals_all, nrow = n_pts, ncol = n_off, byrow = TRUE)

    apply(vals_mat, 1, function(row) {
      v <- row[!is.na(row)]
      if (length(v) == 0) NA_real_ else kernelFun(v)
    })
  }


# Kernel value calculation helper function:
  computeImageValues <- function(x, y) {
    if (length(imageRasters) == 0) return(rep(0, length(x)))
    Reduce("+", lapply(seq_along(imageRasters), function(k) {
      imageBetas[[k]] * extractKernelValues_vec(
        imageRasters[[k]], x, y,
        imageKernelDimensions[k],
        imageKernelFunctions[[k]]
      )
    }))
  }


# Initializing tibble and starting point:
  simData <- tibble::tibble(stepID    = 0:nSteps,
                            x_        = numeric(nSteps + 1),
                            y_        = numeric(nSteps + 1),
                            SL        = numeric(nSteps + 1),
                            Direction = numeric(nSteps + 1),
                            Change    = numeric(nSteps + 1),
                            habitat   = numeric(nSteps + 1))
  simData$x_[1] <- xStart
  simData$y_[1] <- yStart
  simData$SL[1] <- 0
  simData$Direction[1] <- angleStart
  simData$Change[1] <- 0

  numVal_start <- sum(sapply(seq_along(numericRasters), function(k)
    betas[[k]] * terra::extract(numericRasters[[k]], cbind(simData$x_[1], simData$y_[1]))[, 1]))

  imgVal_start <- computeImageValues(simData$x_[1], simData$y_[1])
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

    numLinpred <- Reduce("+", lapply(seq_along(numericRasters), function(k)
      betas[[k]] * terra::extract(numericRasters[[k]], cbind(xToro, yToro))[, 1]))

    imgLinpred <- computeImageValues(xToro, yToro)
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
    simData$habitat[i+1]     <- rawLinpred[chosenStep]

    if (i %% 1000 == 0) message("Step ", i, "/", nSteps)
  }

# Adding timestamp:
  simData$t_ <- seq(from = tStart, by = timeStep, length.out = nrow(simData))
  track <- amt::make_track(simData, x_, y_, t_)
  return(track)
}


#' @title Plot SpatRasterCollection
#'
#' @description Plots all layers of a \code{SpatRasterCollection} in a single overlaid
#' plot Continuous layers are rendered as colour gradients from white to the specified
#' colour; binary layers are rendered as a single transparent colour. All layers are
#' drawn with transparency so overlapping layers remain visible.
#'
#' @param x SpatRasterCollection. The object to plot.
#' @param colours character vector. Colours for each layer, in the same order as the
#'   layers in \code{x}. If omitted, colours are sampled randomly.
#' @param onlyoverlap logical. If TRUE (default), only the spatial overlap of all layers
#'   is plotted. If FALSE, the full extent of all layers is used and non-overlapping
#'   areas appear empty.
#'
#' @return None. Only for producing plot.
#'
#' @examples
#' \dontrun{
#' # see system.file("examples/simulation-example.R", package = "citoMove")
#' }
#'
#' @export
#' @author Mika Schubert

plotSpatRasterCollection <- function(x, colours, onlyoverlap = TRUE) {

  if (!inherits(x, "SpatRasterCollection")) stop("x must be a SpatRasterCollection")

  rasterList <- terra::as.list(x)
  if(missing(colours)) {colours <- sample(grDevices::colors(),length(rasterList))}

  if(onlyoverlap == TRUE){
    xminAll <- max(sapply(rasterList, function(l) terra::ext(l)[1]))
    xmaxAll <- min(sapply(rasterList, function(l) terra::ext(l)[2]))
    yminAll <- max(sapply(rasterList, function(l) terra::ext(l)[3]))
    ymaxAll <- min(sapply(rasterList, function(l) terra::ext(l)[4]))
    extAll  <- terra::ext(xminAll, xmaxAll, yminAll, ymaxAll)
    rasterList <- lapply(rasterList, function(l) terra::crop(l, extAll))

  } else {
    xminAll <- min(sapply(rasterList, function(l) terra::ext(l)[1]))
    xmaxAll <- max(sapply(rasterList, function(l) terra::ext(l)[2]))
    yminAll <- min(sapply(rasterList, function(l) terra::ext(l)[3]))
    ymaxAll <- max(sapply(rasterList, function(l) terra::ext(l)[4]))
    extAll  <- terra::ext(xminAll, xmaxAll, yminAll, ymaxAll)
  }


  terra::plot(rasterList[[1]],
              ext = extAll,
              col = if(length(unique(terra::values(rasterList[[1]]))) == 2){c(grDevices::adjustcolor(NA, alpha = 0), grDevices::adjustcolor(colours[1], alpha = 1/length(rasterList)))}else{grDevices::adjustcolor(grDevices::colorRampPalette(c("white", colours[1]))(100), alpha = 1/length(rasterList))},
              legend = FALSE)
  if(length(rasterList) > 1) {
    for(l in 2:length(rasterList)){
      terra::plot(rasterList[[l]],
                  ext = extAll,
                  col = if(length(unique(terra::values(rasterList[[l]]))) == 2){c(grDevices::adjustcolor(NA, alpha = 0), grDevices::adjustcolor(colours[l], alpha = 1/length(rasterList)))}else{grDevices::adjustcolor(grDevices::colorRampPalette(c("white", colours[l]))(100), alpha = 1/length(rasterList))},
                  legend = FALSE,
                  add = TRUE)
    }}
}
