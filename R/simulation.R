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
                   ext = terra::ext(-0.5 * size, 0.5 * size, -0.5 * size, 0.5 * size))  #create raster with defined habitat size and resolution
  terra::values(r) <- stats::rnorm(terra::ncell(r))  #normal distribution on the raster


  r_smooth <- terra::focal(r, w = terra::focalMat(r, clustering, "Gauss"),
                           na.policy = "omit",
                           expand = TRUE) #smoothing according to neighboring weights

  threshold <- stats::quantile(terra::values(r_smooth), 1-ratio, na.rm=TRUE)  #binarization
  hab <- r_smooth >= threshold

  layer <- tibble::tibble(raster = list(hab), layerColour = colour)
  class(layer) = "citoMoveLayer"

  return(layer)
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
#' @param rasterLayers a list of the raster layers of the habitat vraiables. Must be in form rasterLayers = list(...)
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
                          numericlayers = NULL,
                          imageLayers = NULL,
                          betas = c(-3)){

  if(is.null(numericLayers)) {numericLayers <- list(simulateLayer(plot = FALSE))}

  rasters <- lapply(numericLayers, function(x) x$raster[[1]])
  layerColours <- sapply(numericLayers, function(x) x$layerColour)




  simData <- tibble::tibble(stepID = 0:nSteps,
                        x = numeric(nSteps + 1),
                        y = numeric(nSteps + 1),
                        SL = numeric(nSteps + 1),
                        Direction = numeric(nSteps + 1),
                        Change = numeric(nSteps + 1),
                        habitat = numeric(nSteps + 1))  #allocating data frame



  simData$x[1] <- xStart
  simData$y[1] <- yStart
  simData$SL[1] <- 0
  simData$Direction[1] <- angleStart
  simData$Change[1] <- 0
  simData$habitat[1] <- sum(sapply(seq_along(rasters), function(k) betas[[k]] * terra::extract(rasters[[k]], cbind(simData$x[1], simData$y[1]))[,1])) #simulating starting points




  for(i in 1:nSteps) {

    SLrel <- stats::rgamma(nChoiceSet, shape = shapeSL, scale = scaleSL)
    ChangeRad <- circular::rvonmises(nChoiceSet, mu = circular::circular(0), kappa = kappaTA)
    Change <- as.numeric(ChangeRad) * 180/pi
    TArel <- simData$Direction[i] + Change
    x <- simData$x[i] + SLrel * sin(TArel * pi/180)
    y <- simData$y[i] + SLrel * cos(TArel * pi/180)
    border <- terra::ext(rasters[[1]])
    x <- ((x - border[1]) %% (border[2] - border[1])) + border[1]
    y <- ((y - border[3]) %% (border[4] - border[3])) + border[3]
    Direction <- (atan2(x-simData$x[i], y-simData$y[i]) * 180/pi) %%360

    linpred <- Reduce("+", lapply(seq_along(rasters), function(k) betas[[k]] * terra::extract(rasters[[k]], cbind(x, y))[,1]))

    linpred <- linpred - max(linpred)
    p <- exp(linpred) / sum(exp(linpred))

    multinom <- stats::rmultinom(1, size = 1, prob = p)
    chosenStep <- which(multinom == 1)


    simData$x[i+1] <- x[chosenStep]
    simData$y[i+1] <- y[chosenStep]
    simData$SL[i+1] <- SLrel[chosenStep]
    simData$Direction[i+1] <- Direction[chosenStep]
    simData$Change[i+1] <- Change[chosenStep]
    simData$habitat[i+1] <- sum(sapply(seq_along(rasters), function(k) betas[[k]] * terra::extract(rasters[[k]], cbind(x[chosenStep], y[chosenStep]))[,1]))

    } #simulating tracks

    simData$time <- seq(from = tStart, by = "hour", length.out = nrow(simData))

  track <- tibble::tibble(track = list(simData), trackColour = colour, layers = list(numericLayers))
  class(track) = "citoMoveTrack"

  return(track)
}



#' @title Merge Tracks
#'
#' @description Merges multiple tracks. The habitat variable layers of the tracks have to be identical
#'
#' @export
#' @author Mika Schubert

mergeTracks <- function(...){

  tracks = list(...)

  trackData <- do.call(rbind, lapply(seq_along(tracks), function(i){
    df <- tracks[[i]]$track[[1]]
    df$trackID <- i
    df$trackColour <- tracks[[i]]$trackColour
    df}))

  layers <- tracks[[1]]$layers[[1]]

  simulation <- tibble::tibble(tracks = list(trackData), layer = list(layers))
  class(simulation) <- "citoMoveSimulation"

  return(simulation)
}



#' @title Plot Simulated Objects
#'
#' @description Plots one or more objects of class 'Layer', one or more objects of class 'Track', or a single object of class 'Simulation'.The habitat variable layers of the tracks have to be identical
#'
#' @export
#' @author Mika Schubert
#'
#'

#plot.citoMoveTrack(x, ...)

plot(x)


plotSimulation <- function(...) {

  objects <- list(...)

  plotLayers <- function(layers) {
    rasters <- lapply(layers, function(x) x$raster[[1]])
    layerColours <- sapply(layers, function(x) x$layerColour)

    terra::plot(rasters[[1]], col = c(NA, layerColours[1]),
                legend = FALSE, alpha = 1/length(rasters))
    if(length(rasters) > 1) {
      for(l in 2:length(rasters)){
        terra::plot(rasters[[l]], col = c(NA, layerColours[l]),
                    legend = FALSE, alpha = 1/length(rasters), add = TRUE)}}
  } # helper function to plot single or multiple layers


  plotTrack <- function(trackData, trackColour) {
    lines(trackData$x, trackData$y, col = trackColour)
    points(trackData$x[1], trackData$y[1], pch = 16, col = trackColour)
  } # helper function to plot single track



  if(all(sapply(objects, function(o) inherits(o, "citoMoveLayer")))) {
    plotLayers(objects)
  } # case 1: one or multiple layers


  else if(all(sapply(objects, function(o) inherits(o, "citoMoveTrack")))) {
    layers <- objects[[1]]$layers[[1]]
    plotLayers(layers)

    for(i in seq_along(objects)){
      trackData <- objects[[i]]$track[[1]]
      trackColour <- objects[[i]]$trackColour
      plotTrack(trackData, trackColour)}
  } # case 2: one or multiple tracks


  else if(length(objects) == 1 && inherits(objects[[1]], "citoMoveSimulation")) {
    simulation <- objects[[1]]
    layers <- simulation$layer[[1]]
    plotLayers(layers)

    trackData <- simulation$tracks[[1]]
    trackIDs <- unique(trackData$trackID)

    for(m in trackIDs){
      dat <- trackData[trackData$trackID == m, ]
      trackColour <- dat$trackColour[1]  # Farbe aus den Daten
      plotTrack(dat, trackColour)}
  } # case 3: a single simulation

  else {
    stop("Input must be one or more objects of class 'Layer', one or more objects of class 'Track', or a single object of class 'Simulation'")
  }
}
