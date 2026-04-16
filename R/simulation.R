
#' @title Simulate Animal Tracks
#'
#' @description Generates a simulated animal track using the SSF method.
#'
#' @param xStart starting x coordinate of the animal
#' @param yStart starting y coordinate of the animal
#' @param tStart starting time of the animal as POSIXct
#' @param angleStart starting angle clockwise from due north
#' @param nSteps the length of the simulated track as number of steps
#' @param nChoiceSet the size of the choice set per step
#' @param scaleSL the scale parameter of the gamma distribution of the step length
#' @param shapeSL the shape parameter of the gamma distribution of the step length
#' @param kappaTA the kappa parameter of the von mises distribution of the turning angle
#' @param colour colour of the track for plotting
#' @param numericLayers a list of the numeric raster layers
#' @param rasterLayers a list of the raster layers of the habitat variables. Must be in form rasterLayers = list(...)
#' @param betas a vector of the preference values of the habitat variables. Must be same length as the list of numericLayers
#' @param imageKernelsDimensions description
#'
#' @details
#' Examplary values as default: xStart = 0, yStart = 0, tStart = as.POSIXct("2026-01-01 00:00:00",tz = "UTC"), angleStart = 0, nSteps = 100, nChoiceSet = 10, scaleSL = 2, shapeSL = 2, kappaTA = 2, numericLayers = list(simulateLayer()), betas = c(-3)
#'
#' @example inst/examples/simulation-example.R
#'
#' @export
#' @author Mika Schubert


simulateTrack <- function(xStart = 0,
                          yStart = 0,
                          tStart = as.POSIXct("2026-01-01 00:00:00",tz = "UTC"),
                          angleStart = 0,
                          nSteps = 100,
                          nChoiceSet = 10,
                          scaleSL = 2,
                          shapeSL = 2,
                          kappaTA = 2,
                          numericLayers = NULL,
                          betas = c(-3),
                          imageLayers = NULL,
                          imageKernelDimensions = c(3),
                          imageKernelFunctions = list(f1 = mean))
{

  if (is.null(numericLayers)) stop("numericLayers must be provided")
  if (!inherits(numericLayers, "SpatRasterCollection")) stop("numericLayers must be of class 'SpatRasterCollection'")
  if (!inherits(tStart, "POSIXct")) stop("tStart must be of class 'POSIXct'")

  rasterList <- terra::as.list(numericLayers)

  xminAll <- max(sapply(rasterList, function(l) terra::ext(l)[1]))
  xmaxAll <- min(sapply(rasterList, function(l) terra::ext(l)[2]))
  yminAll <- max(sapply(rasterList, function(l) terra::ext(l)[3]))
  ymaxAll <- min(sapply(rasterList, function(l) terra::ext(l)[4]))
  extAll  <- terra::ext(xminAll, xmaxAll, yminAll, ymaxAll)
  rasters <- lapply(rasterList, function(l) terra::crop(l, extAll))

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
  simData$habitat[1] <- sum(sapply(seq_along(rasters), function(k)
    betas[[k]] * terra::extract(rasters[[k]], cbind(simData$x_[1], simData$y_[1]))[,1]))

  for(i in 1:nSteps) {

    SLrel     <- stats::rgamma(nChoiceSet, shape = shapeSL, scale = scaleSL)
    ChangeRad <- circular::rvonmises(nChoiceSet, mu = circular::circular(0), kappa = kappaTA)
    Change    <- as.numeric(ChangeRad) * 180/pi
    TArel     <- simData$Direction[i] + Change
    x         <- simData$x_[i] + SLrel * sin(TArel * pi/180)
    y         <- simData$y_[i] + SLrel * cos(TArel * pi/180)
    border    <- terra::ext(rasters[[1]])
    x         <- ((x - border[1]) %% (border[2] - border[1])) + border[1]
    y         <- ((y - border[3]) %% (border[4] - border[3])) + border[3]
    Direction <- (atan2(x - simData$x_[i], y - simData$y_[i]) * 180/pi) %% 360

    linpred <- Reduce("+", lapply(seq_along(rasters), function(k)
      betas[[k]] * terra::extract(rasters[[k]], cbind(x, y))[,1]))
    linpred <- linpred - max(linpred)
    p       <- exp(linpred) / sum(exp(linpred))

    multinom   <- stats::rmultinom(1, size = 1, prob = p)
    chosenStep <- which(multinom == 1)

    simData$x_[i+1]        <- x[chosenStep]
    simData$y_[i+1]        <- y[chosenStep]
    simData$SL[i+1]        <- SLrel[chosenStep]
    simData$Direction[i+1] <- Direction[chosenStep]
    simData$Change[i+1]    <- Change[chosenStep]
    simData$habitat[i+1]   <- sum(sapply(seq_along(rasters), function(k)
      betas[[k]] * terra::extract(rasters[[k]], cbind(x[chosenStep], y[chosenStep]))[,1]))

  }

  simData$t_ <- seq(from = tStart, by = "hour", length.out = nrow(simData))

  track <- amt::make_track(simData, x_, y_, t_)

  return(track)
}


#' @title Plot SpatRasterCollection
#'
#' @description Plots an object of class 'SpatRasterCollection'
#'
#' @param x object of class 'SpatRasterCollection'
#' @param colours vector of colours of the layers. Must be same length as number of layers
#' @param onlyoverlap if TRUE only overlap of all layers will be plotted. If FALSE whole layers will be plotted
#'
#' @example inst/examples/simulation-example.R
#'
#' @export
#' @author Mika Schubert

plotSpatRasterCollection <- function(x, colours, onlyoverlap = TRUE) {

  rasterList <- terra::as.list(x)
  if(missing(colours)) {colours <- sample(colors(),length(rasterList))}

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
              col = if(length(unique(terra::values(rasterList[[1]]))) == 2){c(adjustcolor(NA, alpha = 0), adjustcolor(colours[1], alpha = 1/length(rasterList)))}else{adjustcolor(colorRampPalette(c("white", colours[1]))(100), alpha = 1/length(rasterList))},
              legend = FALSE)
  if(length(rasterList) > 1) {
    for(l in 2:length(rasterList)){
      terra::plot(rasterList[[l]],
                  ext = extAll,
                  col = if(length(unique(terra::values(rasterList[[l]]))) == 2){c(adjustcolor(NA, alpha = 0), adjustcolor(colours[l], alpha = 1/length(rasterList)))}else{adjustcolor(colorRampPalette(c("white", colours[l]))(100), alpha = 1/length(rasterList))},
                  legend = FALSE,
                  add = TRUE)
    }}
}
