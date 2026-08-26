# Example workflow for simulateTrack()

# This example shows how to:
#   1. Create different kinds of Rasters with terra and combine them to a
#      SpatRasterCollection
#   2. (Optionally) look at all involved layers overlaid in a single plot,
#      built "by hand" with terra::plot()
#   3. Simulate an animal track with simulateTrack()

# To simulate a track we need at least one object of class SpatRasterCollection
# The object has to obtain one or multiple objects of class SpatRaster
# Rasters can be analysed as numeric or image layers (see description simulateTrack)
# and should therefore be sorted in corresponding SpatRasterCollections

# Example for simulating two numeric raster layers with the terra package,
# combining them in an object of SpatRasterCollection, and one
# SpatRasterCollection for one image layer:

# Defining starting point of track as center of raster layers (e.g. EPSG:25833)
centerX <- 820778.84
centerY <- 5433014.65

# The first numeric layer is a layer with a continuous variable:
layer1 <- terra::rast(
  ncols = 300,
  nrows = 450,
  ext   = terra::ext(centerX - 100, centerX + 100,   # defining extent and resolution of the layer
                     centerY - 150, centerY + 150)   # with resolution being number of cells per map unit
)
terra::crs(layer1) <- "EPSG:25833"

terra::values(layer1) <- stats::rnorm(terra::ncell(layer1))

layer1 <- terra::focal(
  layer1,
  w         = terra::focalMat(layer1, 3, "Gauss"),
  na.policy = "omit",
  expand    = TRUE
)  # sampling values for the cells and smoothening them with terra::focalMat function

terra::plot(
  layer1,
  col    = colorRampPalette(c("white", "darkblue"))(100),
  legend = FALSE
)  # plot (just for visualization)


# The second numeric raster is a layer with a binary variable:
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
layer2    <- as.numeric(layer2 >= threshold)  # implementing a threshold for binary values

terra::plot(
  layer2,
  col    = c(adjustcolor(NA, alpha = 0), "darkgreen"),
  legend = FALSE
)  # plot (just for visualization)


# The two numeric layers of class SpatRaster are combined in an object of class SpatRasterCollection
numLayers <- terra::sprc(layer1, layer2)


# The image layer is transformed as a raster with the terra package:
# For this example we simulate a raster (dummyImage) as image layer:

# An exemplary workflow for real images would be:
#  img      <- terra::rast("path/to/your/image")
#  img_gray <- img[[1]] * 0.299 + img[[2]] * 0.587 + img[[3]] * 0.114
#  img_gray <- terra::flip(img_gray, direction = "vertical")
#  img_gray <- max(terra::values(img_gray), na.rm = TRUE) - img_gray
#  terra::ext(img_gray) <- terra::ext(-200, 200, -300, 300)
#  terra::plot(
#    img_gray,
#    col    = colorRampPalette(c("white", "black"))(100),
#    legend = FALSE
#  )
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
  col    = colorRampPalette(c("white", "black"))(100),
  legend = FALSE
)

imgLayers <- terra::sprc(dummyImage)


# To get a quick visual overview of how all layers relate to each other in
# space, they can simply be plotted on top of each other "by hand":
#   1. Collect all layers (numeric + image) in one list
#   2. Plot the first layer with some transparency (alpha) so the layers
#      underneath stay visible
#   3. Add every following layer to the same plot with add = TRUE
# This replaces the previous plotSpatRasterCollection() helper function -
# for a one-off overview plot like this, doing it manually is short enough
# that a dedicated function is not needed.

allLayers <- list(layer1, layer2, dummyImage)
colours   <- c("darkblue", "darkgreen", "black")

# Restrict the plot to the area where all three layers overlap, since that is
# also the area simulateTrack() will actually simulate on:
xminAll <- max(sapply(allLayers, function(l) terra::ext(l)[1]))
xmaxAll <- min(sapply(allLayers, function(l) terra::ext(l)[2]))
yminAll <- max(sapply(allLayers, function(l) terra::ext(l)[3]))
ymaxAll <- min(sapply(allLayers, function(l) terra::ext(l)[4]))
extAll  <- terra::ext(xminAll, xmaxAll, yminAll, ymaxAll)

terra::plot(
  terra::crop(allLayers[[1]], extAll),
  ext    = extAll,
  col    = colorRampPalette(c("white", colours[1]))(100),
  alpha  = 1 / length(allLayers),
  legend = FALSE
)
for (l in 2:length(allLayers)) {
  terra::plot(
    terra::crop(allLayers[[l]], extAll),
    ext    = extAll,
    col    = colorRampPalette(c("white", colours[l]))(100),
    alpha  = 1 / length(allLayers),
    legend = FALSE,
    add    = TRUE
  )
}


# The track is simulated with this function.
#   - numericLayers must be an object of class SpatRasterCollection containing
#     one or multiple SpatRaster.
#   - betas must be a vector of preference values for each of the layers.
#     Must be same length as number of layers.
track1 <- simulateTrack(
  xStart               = centerX,
  yStart               = centerY,
  crs                  = "EPSG:25833",
  tStart               = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
  timeStep             = "hour",
  angleStart           = 0,
  nSteps               = 100,
  nChoiceSet           = 100,
  scaleSL              = 2,
  shapeSL              = 2,
  kappaTA              = 2,
  numericLayers        = numLayers,
  betas                = c(2, -1),
  imageLayers          = imgLayers,
  imageKernelDimensions = c(3),
  imageKernelFunctions = list(f1 = mean),
  imageBetas           = c(3),
  scaleNumericLayers   = TRUE,
  scaleImageLayers     = TRUE
)

head(track1)

# To visualize the track, the points and/or lines function can be used,
# adding the track to a beforehand plotted layer or raster overlay.
# Other tracks of the amt class track can be added to the plot in the same way.
points(track1$x_[1], track1$y_[1], pch = 16, col = "red")
lines(track1$x_, track1$y_, col = "red")
