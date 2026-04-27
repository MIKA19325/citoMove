# To simulate a track we need at least one object of class SpatRasterCollection
# The object has to obtain one or multiple objects of class SpatRaster
# Rasters can be analysed as numeric or raster layers (see description simulateTrack)
# and should therefore be sorted in corresponding SpatRasterCollections


# Example for simulating two numeric raster layers with the terra package,
# combining them in an object of SpatRasterCollection and plotting them with the
# plotSpatRasterCollection function and one SpatRasterCollection for one image layer:

# The first numeric layer is a layer with a continuous variable:
layer1 <- terra::rast(
  ncols = 300,
  nrows = 450,
  ext   = terra::ext(-100, 100, -150, 150)  # defining extent and resolution of the layer
)                                            # with resolution being number of cells per map unit

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
  ext   = terra::ext(-150, 150, -100, 100)
)

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
# For real images:

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


# For this example we simulate a raster as image layer:
dummyImage <- terra::rast(
  ncols = 25,
  nrows = 25,
  ext   = terra::ext(-200, 200, -200, 200)
)

terra::values(dummyImage) <- runif(terra::ncell(dummyImage))

terra::plot(
  dummyImage,
  col    = colorRampPalette(c("white", "black"))(100),
  legend = FALSE
)

imgLayers <- terra::sprc(dummyImage)

# The SpatRasterCollection is plotted with this function.
# Shows only the area where all layers overlap when onlyoverlap = TRUE,
# or the whole rasters when onlyoverlap = FALSE.
allLayers <- terra::sprc(c(as.list(numLayers), as.list(imgLayers)))

plotSpatRasterCollection(
  allLayers,
  colours     = c("darkblue", "darkgreen", "black"),
  onlyoverlap = TRUE
)


# The track is simulated with this function.
#   - numericLayers must be an object of class SpatRasterCollection containing
#     one or multiple SpatRaster.
#   - betas must be a vector of preference values for each of the layers.
#     Must be same length as number of layers.
track1 <- simulateTrack(
  xStart               = 0,
  yStart               = 0,
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


# To visualize the track, the points and/or lines function can be used,
# adding the track to a beforehand plotted layer or SpatRasterCollection.
# Other tracks of the amt class track can be added to the plot in the same way.
points(track1$x_[1], track1$y_[1], pch = 16, col = "red")
lines(track1$x_, track1$y_, col = "red")
