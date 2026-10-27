# Example workflow for simulateTrack()

# This example shows how to:
#   1. Create numeric and image raster layers with terra and combine them
#      into SpatRasterCollections
#   2. (Optionally) look at all layers overlaid in a single plot, built
#      "by hand" with terra::plot()
#   3. Simulate an animal track with simulateTrack()

# simulateTrack() needs at least one SpatRasterCollection containing one or
# several SpatRasters. Layers are evaluated either as numeric layers (value of
# a single cell at the step end point) or as image layers (aggregated values of
# a window of cells around the end point, see ?simulateTrack), and are
# therefore sorted into two separate SpatRasterCollections.

set.seed(42)

# Starting point of the track, used as centre of all layers (EPSG:25833):
centerX <- 820778.84
centerY <- 5433014.65

# The first numeric layer contains a continuous variable:
layer1 <- terra::rast(
  ncols = 300,
  nrows = 450,
  ext   = terra::ext(centerX - 100, centerX + 100,   # extent of the layer; together with
                     centerY - 150, centerY + 150)   # ncols/nrows it sets the resolution
)                                                    # (here 200 m / 300 cells = 0.67 m per cell)
terra::crs(layer1) <- "EPSG:25833"

terra::values(layer1) <- stats::rnorm(terra::ncell(layer1))

layer1 <- terra::focal(
  layer1,
  w         = terra::focalMat(layer1, 3, "Gauss"),
  na.policy = "omit",
  expand    = TRUE
)  # random cell values, smoothed with a Gaussian filter

terra::plot(
  layer1,
  col    = grDevices::colorRampPalette(c("white", "darkblue"))(100),
  legend = FALSE
)  # plot (just for visualization)


# The second numeric layer contains a binary variable:
layer2 <- terra::rast(
  ncols = 300,
  nrows = 200,
  ext   = terra::ext(centerX - 150, centerX + 150,
                     centerY - 100, centerY + 100)
)
terra::crs(layer2) <- "EPSG:25833"

terra::values(layer2) <- stats::rnorm(terra::ncell(layer2))

layer2 <- terra::focal(
  layer2,
  w         = terra::focalMat(layer2, 15, "Gauss"),
  na.policy = "omit",
  expand    = TRUE
)

threshold <- stats::quantile(terra::values(layer2), 0.5, na.rm = TRUE)
layer2    <- as.numeric(layer2 >= threshold)  # threshold for binary values

terra::plot(
  layer2,
  col    = c(grDevices::adjustcolor(NA, alpha.f = 0), "darkgreen"),
  legend = FALSE
)  # plot (just for visualization)


# Both numeric layers are combined into one SpatRasterCollection:
numLayers <- terra::sprc(layer1, layer2)


# The image layer is also a terra raster. For this example we simulate a
# raster (dummyImage) as image layer.

# With a real, georeferenced image (in the same CRS as the other layers), a
# workflow could look like this:
#  img      <- terra::rast("path/to/your/image.tif")
#  img_gray <- img[[1]] * 0.299 + img[[2]] * 0.587 + img[[3]] * 0.114  # RGB to grayscale
#  img_gray <- terra::global(img_gray, "max", na.rm = TRUE)[1, 1] - img_gray
#              # (optional) invert, so that dark pixels get high values
#  names(img_gray) <- "brightness"
#  imgLayers <- terra::sprc(img_gray)

dummyImage <- terra::rast(
  ncols = 25,
  nrows = 25,
  ext   = terra::ext(centerX - 200, centerX + 200,
                     centerY - 200, centerY + 200)
)
terra::crs(dummyImage) <- "EPSG:25833"

terra::values(dummyImage) <- stats::runif(terra::ncell(dummyImage))

terra::plot(
  dummyImage,
  col    = grDevices::colorRampPalette(c("white", "black"))(100),
  legend = FALSE
)

imgLayers <- terra::sprc(dummyImage)


# To get a quick visual overview of how all layers relate to each other in
# space, they can be plotted on top of each other:
#   1. Collect all layers (numeric + image) in one list
#   2. Plot the first layer with some transparency (alpha) so that the layers
#      underneath stay visible
#   3. Add every following layer to the same plot with add = TRUE

allLayers <- list(layer1, layer2, dummyImage)
colours   <- c("darkblue", "darkgreen", "black")

# Restrict the plot to the area where all three layers overlap, since this is
# also the area simulateTrack() simulates on:
xminAll <- max(sapply(allLayers, function(l) terra::ext(l)[1]))
xmaxAll <- min(sapply(allLayers, function(l) terra::ext(l)[2]))
yminAll <- max(sapply(allLayers, function(l) terra::ext(l)[3]))
ymaxAll <- min(sapply(allLayers, function(l) terra::ext(l)[4]))
extAll  <- terra::ext(xminAll, xmaxAll, yminAll, ymaxAll)

terra::plot(
  terra::crop(allLayers[[1]], extAll),
  ext    = extAll,
  col    = grDevices::colorRampPalette(c("white", colours[1]))(100),
  alpha  = 1 / length(allLayers),
  legend = FALSE
)
for (l in 2:length(allLayers)) {
  terra::plot(
    terra::crop(allLayers[[l]], extAll),
    ext    = extAll,
    col    = grDevices::colorRampPalette(c("white", colours[l]))(100),
    alpha  = 1 / length(allLayers),
    legend = FALSE,
    add    = TRUE
  )
}


# Simulate the track:
#   - numericLayers / imageLayers must be SpatRasterCollections containing one
#     or several SpatRasters.
#   - betas / imageBetas must contain one selection coefficient per layer.
track1 <- simulateTrack(
  xStart                = centerX,
  yStart                = centerY,
  crs                   = "EPSG:25833",
  tStart                = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
  timeStep              = "hour",
  angleStart            = 0,
  nSteps                = 100,
  nChoiceSet            = 100,
  scaleSL               = 2,
  shapeSL               = 2,
  kappaTA               = 2,
  numericLayers         = numLayers,
  betas                 = c(2, -1),
  imageLayers           = imgLayers,
  imageKernelDimensions = c(3),
  imageKernelFunctions  = list(f1 = mean),
  imageBetas            = c(3),
  scaleNumericLayers    = TRUE,
  scaleImageLayers      = TRUE
)

head(track1)

# To visualize the track, add it with graphics::points() and/or
# graphics::lines() to a previously plotted layer or overlay. Other amt tracks
# can be added in the same way. Note that the track is a continuous path and
# can leave the plotted area (see "Boundaries" in ?simulateTrack).
graphics::points(track1$x_[1], track1$y_[1], pch = 16, col = "red")
graphics::lines(track1$x_, track1$y_, col = "red")

# The same function can also simulate on a landscape without toroidal
# wrapping (torus = FALSE): candidate steps outside the layers cannot be
# chosen, so the track stays inside the layers. With kernelAsMatrix = TRUE,
# the kernel function receives the whole window as a matrix (row 1 = north,
# column 1 = west) and can therefore react to the spatial arrangement of the
# cells, here the contrast between the centre cell and its 8 neighbours.
# With returnChoiceSets = TRUE all candidate steps are stored in the
# attribute "choiceSets" of the track.
centreContrast <- function(m) m[2, 2] - mean(m[-5])   # for a 3 x 3 window

track2 <- simulateTrack(
  xStart                = centerX,
  yStart                = centerY,
  crs                   = "EPSG:25833",
  nSteps                = 50,
  numericLayers         = numLayers,
  betas                 = c(2, -1),
  imageLayers           = imgLayers,
  imageKernelDimensions = c(3),
  imageKernelFunctions  = list(f1 = centreContrast),
  imageBetas            = c(3),
  torus                 = FALSE,
  kernelAsMatrix        = TRUE,
  returnChoiceSets      = TRUE
)

choiceSets <- attr(track2, "choiceSets")
head(choiceSets)
