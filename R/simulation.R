#' @title Simulate Habitat Layer
#'
#' @description Generates a simulated habitat layer with random fragments of one binary habitat variable.
#'
#' @param size square side length of the simulated layer with the coordinates (0,0) in the middle of the square. Must be a positive value
#' @param resolution resolution of the simulated layer
#' @param clustering clustering of the habitat variable. Must be smaller then 1/3 of the size.
#' @param ratio ratio of the layer area covered by the habitat
#' @param plot if TRUE plot will be generated, if FALSE no plot will be generated
#' @param colour colour of the layer in the plot
#'
#' @details
#' Examplary values as default: size = 200, resolution = 1, clustering = 15, ratio = 0.5, plot = TRUE, colour = "darkgreen"
#' As computing time scales directly with number of cells and number of cells grows exponentially with size as well as resolution, these parameters are to be kept as small as possible.
#' The clustering is computed as the standard deviation of a Gauss distribution in map units and therefore must be significantly (max. 3 times smaller) than the size of the raster layer. Big clustering values also extend computing time.
#'
#' @export
#' @author Mika Schubert

simulateLayer <- function(size = 200,
                          resolution = 1,
                          clustering = 15,
                          ratio = 0.5,
                          colour = "darkgreen"){

  resolution <- 1/resolution
  r <- terra::rast(ncols = size / resolution,
                   nrows = size / resolution,
                   ext = terra::ext(-0.5 * size, 0.5 * size, -0.5 * size, 0.5 * size))
  terra::values(r) <- stats::rnorm(terra::ncell(r))

  r_smooth <- terra::focal(r, w = terra::focalMat(r, clustering, "Gauss"),
                           na.policy = "omit",
                           expand = TRUE)

  threshold <- stats::quantile(terra::values(r_smooth), 1-ratio, na.rm=TRUE)
  hab <- r_smooth >= threshold

  names(hab) <- colour
  attr(hab, "layerColour") <- colour

  return(hab)
}



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
#'
#' @details
#' Examplary values as default: xStart = 0, yStart = 0, tStart = as.POSIXct("2026-01-01 00:00:00",tz = "UTC"), angleStart = 0, nSteps = 100, nChoiceSet = 10, scaleSL = 2, shapeSL = 2, kappaTA = 2, numericLayers = list(simulateLayer()), betas = c(-3)
#'
#' @example inst/examples/
#'
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
                          colour = "black",
                          numericLayers = NULL,
                          imageLayers = NULL,
                          betas = c(-3)){

  if(is.null(numericLayers)) {numericLayers <- list(simulateLayer())}

  rasters <- numericLayers

  layerColours <- sapply(numericLayers, function(x) attr(x, "layerColour"))

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

  track <- amt::make_track(simData, x_, y_, t_,
                           SL = SL, Direction = Direction,
                           Change = Change, habitat = habitat,
                           stepID = stepID)

  attr(track, "trackColour") <- colour
  attr(track, "layers")      <- numericLayers

  return(track)
}



#' @title Merge Tracks
#'
#' @description Merges multiple tracks. The habitat variable layers of the tracks have to be identical
#'
#' @export
#' @author Mika Schubert

mergeTracks <- function(...){

  tracks <- list(...)

  trackData <- do.call(rbind, lapply(seq_along(tracks), function(i){
    df             <- as.data.frame(tracks[[i]])
    df$trackID     <- i
    df$trackColour <- attr(tracks[[i]], "trackColour")
    df
  }))

  layers <- attr(tracks[[1]], "layers")

  simulation <- tibble::tibble(tracks = list(trackData), layer = list(layers))
  class(simulation) <- "citoMoveSimulation"

  return(simulation)
}



#' @title Plot Simulated Objects
#'
#' @description Plots a single object of class 'citoMoveSimulation'.
#'
#' @param x object of class citoMoveSimulation
#'
#' @export
#' @author Mika Schubert

plot.citoMoveSimulation <- function(x) {

  layers    <- x$layer[[1]]
  trackData <- x$tracks[[1]]
  trackIDs  <- unique(trackData$trackID)
  layerColours <- sapply(layers, function(l) attr(l, "layerColour"))

  terra::plot(layers[[1]], col = c(NA, layerColours[1]),
              legend = FALSE, alpha = 1/length(layers))
  if(length(layers) > 1) {
    for(l in 2:length(layers)){
      terra::plot(layers[[l]], col = c(NA, layerColours[l]),
                  legend = FALSE, alpha = 1/length(layers), add = TRUE)
    }
  }

  for(m in trackIDs){
    dat         <- trackData[trackData$trackID == m, ]
    trackColour <- dat$trackColour[1]
    lines(dat$x_, dat$y_, col = trackColour)
    points(dat$x_[1], dat$y_[1], pch = 16, col = trackColour)
  }
}


