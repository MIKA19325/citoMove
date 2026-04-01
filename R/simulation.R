library(terra)  #for raster operations
library(circular) #for von mises operations in handling turning angle


#' @title Simulate Habitat Layer
#'
#' @description Generates a simulated habitat layer with random fragments of one binary habitat variable.
#'
#' @param habitatParameters a list of 4 habitat parameters, must be in form list(size=...,resolution=...,fragmentation=...,ratio=...)
#' @param size square side length of the simulated layer with the coordinates (0,0) in the middle of the square
#' @param resolution resolution of the simulated layer
#' @param fragmentation fragmentation of the habitat
#' @param ratio ratio of the layer area covered by the habitat
#'
#' @author Mika Schubert

simulateLayer <- function(habitatParameters){

  if(!is(habitatParameters, "list")) stop("parameter 'habitatParameters' has to be a list of form list(size=...,resolution=...,fragmentation=...,ratio=...)")
  if(!all(c("size","resolution","fragmentation","ratio") %in% names(habitatParameters))) stop("form of list 'habitatParameters' has to be list(size=...,resolution=...,fragmentation=...,ratio=...)")

  r <- rast(ncols = habitatParameters$size / habitatParameters$resolution,
            nrows = habitatParameters$size / habitatParameters$resolution,
            ext = ext(-0.5 * habitatParameters$size, 0.5 * habitatParameters$size, -0.5 * habitatParameters$size, 0.5 * habitatParameters$size))  #create raster with defined habitat size and resolution
  values(r) <- rnorm(ncell(r))  #normal distribution on the raster

  r_smooth <- focal(r, w=focalMat(r, habitatParameters$fragmentation, "Gauss"),
                    na.policy = "omit",
                    expand = TRUE) #smoothing according to neighboring weights

  threshold <- quantile(values(r_smooth), 1-habitatParameters$ratio, na.rm=TRUE)  #binarization
  hab <- r_smooth >= threshold


  plot(hab, col = c("lightyellow", "darkgreen"), legend = FALSE)

  return(hab)
}



#' @title Simulate Animal Tracks
#'
#' @description Generates a simulated animal track using the SSF method.
#'
#' @param simulationParameters a list of the following 5 simulation parameters
#' @param nSteps the length of the simulated track as number of steps
#' @param nChoiceSet the size of the choice set per step
#' @param scaleSL the scale parameter of the gamma distribution of the step length
#' @param shapeSL the shape parameter of the gamma distribution of the step length
#' @param kappaTA the kappa parameter of the von mises distribution of the turning angle
#'
#' @param startParameters a data.frame with 4 columns for the following starting parameters and one row per individual
#' @param x starting x coordinate of the animal
#' @param y starting y coordinate of the animal
#' @param t starting time of the animal as POSIXct
#' @param angle starting angle clockwise from due north
#'
#' @param numericLayers a data frame with the numeric raster layers and their preference values. Must be form data.frame(rasterLayer = I(list(...),beta = ...) to enable ratser data in data frames.
#' @param rasterLayer a raster layer of the habitat vraiable. Must be in form rasterLayer = I(list(...)
#' @param beta preference value of the habitat variable
#'
#' @author Mika Schubert


simulateTrack <- function(simulationParameters  = list(nSteps = 10,
                                                       nChoiceSet = 10,
                                                       scaleSL = 2,
                                                       shapeSL = 2,
                                                       kappaTA = 2),
                          startParameters = data.frame(x=0,
                                                       y=0,
                                                       t=0,
                                                       angle=0),

                          numericLayers = data.frame(rasterLayer = NULL,
                                                     beta = 0),
                          imageLayers = NULL ){

  if(!is(startParameters, "data.frame")) stop("parameter 'startParameters' has to be a dataframe")
  if(!all(c("x","y","t","angle") %in% colnames(startParameters))) stop("form of dataframe 'startParameters' has to be 'x', 'y', 't', 'angle'")
  if(!is(simulationParameters, "list")) stop("parameter 'simulationParameters' has to be a list")
  if(!all(c("nSteps","nChoiceSet","scaleSL","shapeSL","kappaTA") %in% names(simulationParameters))) stop("form of list 'simulationParameters' has to be 'nSteps','nChoiceSet','scaleSL','shapeSL','kappaTA'")
  if(!is(numericLayers, "data.frame")) stop("parameter 'numericLayers' has to be a dataframe")
  if(!all(c("rasterLayer","beta") %in% colnames(numericLayers))) stop("form of dataframe 'numericLayers' has to be 'rasterLayers', 'beta'")
  if(!is(numericLayers$rasterLayer[[1]], "SpatRaster")) stop("object 'rasterLayer' has to be a raster")

  allData <- list()  # Liste zum Speichern aller Tracks

  #allocating data frame

  for(j in 1:nrow(startParameters)){

    simData <- data.frame(stepID = 0:simulationParameters$nSteps,
                          x = numeric(simulationParameters$nSteps + 1),
                          y = numeric(simulationParameters$nSteps + 1),
                          SL = numeric(simulationParameters$nSteps + 1),
                          Direction = numeric(simulationParameters$nSteps + 1),
                          Change = numeric(simulationParameters$nSteps + 1),
                          habitat = numeric(simulationParameters$nSteps + 1))
    simData$ID <- j

    #simulating starting points

    simData$x[1] <- startParameters$x[j]
    simData$y[1] <- startParameters$y[j]
    simData$SL[1] <- 0
    simData$Direction[1] <- startParameters$angle[j]
    simData$Change[1] <- 0

    simData$habitat[1] <- extract(numericLayers$rasterLayer[[1]], cbind(simData$x[1], simData$y[1]))[,1]


    #simulating tracks

    for(i in 1:simulationParameters$nSteps) {

      SLrel <- rgamma(simulationParameters$nChoiceSet, shape = simulationParameters$shapeSL, scale = simulationParameters$scaleSL)
      ChangeRad <- rvonmises(simulationParameters$nChoiceSet, mu = circular(0), kappa = simulationParameters$kappaTA)
      Change <- as.numeric(ChangeRad) * 180/pi
      TArel <- simData$Direction[i] + Change
      x <- simData$x[i] + SLrel * sin(TArel * pi/180)
      y <- simData$y[i] + SLrel * cos(TArel * pi/180)
      ext <- ext(numericLayers$rasterLayer[[1]])
      x <- ((x - ext[1]) %% (ext[2] - ext[1])) + ext[1]
      y <- ((y - ext[3]) %% (ext[4] - ext[3])) + ext[3]
      Direction <- (atan2(x-simData$x[i], y-simData$y[i]) * 180/pi) %%360

      hab <- extract(numericLayers$rasterLayer[[1]], cbind(x, y))[,1]

      linpred <- numericLayers$beta[[1]] * hab
      linpred <- linpred - max(linpred)
      p <- exp(linpred) / sum(exp(linpred))

      multinom <- rmultinom(1, size = 1, prob = p)
      chosenStep <- which(multinom == 1)


      simData$x[i+1] <- x[chosenStep]
      simData$y[i+1] <- y[chosenStep]
      simData$SL[i+1] <- SLrel[chosenStep]
      simData$Direction[i+1] <- Direction[chosenStep]
      simData$Change[i+1] <- Change[chosenStep]
      simData$habitat[i+1] <- hab[chosenStep]

    }

    simData$time <- seq(from = startParameters$t[j],by = "hour", length.out = nrow(simData))

    allData[[j]] <- simData

  }

  simData_all <- do.call(rbind, allData)

  # plot

  plot(numericLayers$rasterLayer[[1]], col=c("lightyellow","darkgreen"))
  for(id in unique(simData_all$ID)){
    dat <- subset(simData_all, ID==id)
    lines(dat$x, dat$y)
    points(dat$x[1], dat$y[1], pch=16, col = "red")
  }

  return(simData_all)
}


a <- list(
  size = 500,
  resolution = 1,
  fragmentation = 15,
  ratio = 0.4)

b <- list(
  nSteps = 300,
  nChoiceSet = 20,
  scaleSL = 1.6,
  shapeSL = 3,
  kappaTA = 3)

c <- data.frame(
  x = c(0),
  y = c(0),
  t = as.POSIXct(c("2026-01-01 00:00:00"), tz = "UTC"),
  angle = c(0))


d <- data.frame(
  rasterLayer = I(list(simulateLayer(a))),
  beta = c(-3))


simulation <- simulateTrack(simulationParameters = b, startParameters = c, numericLayers = d)
