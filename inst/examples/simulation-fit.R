library(amt)
library(lubridate)
library(circular)
library(terra)
library(cito)
library(citoMove)


layer1 <- terra::rast(ncols = 600, nrows = 600,
                      ext = terra::ext(-300, 300, -200, 200))
terra::values(layer1) <- stats::rnorm(terra::ncell(layer1))
layer1 <- terra::focal(layer1, w = terra::focalMat(layer1, 3, "Gauss"),
                       na.policy = "omit", expand = TRUE)

terra::plot(layer1, col = colorRampPalette(c("white", "darkblue"))(100), legend = FALSE)


layer2 <- terra::rast(ncols = 500, nrows = 500,
                      ext = terra::ext(-250, 250, -250, 250))
terra::values(layer2) <- stats::rnorm(terra::ncell(layer2))
layer2 <- terra::focal(layer2, w = terra::focalMat(layer2, 15, "Gauss"),
                       na.policy = "omit", expand = TRUE)
threshold <- stats::quantile(terra::values(layer2), 0.5, na.rm=TRUE)
layer2 <- as.numeric(layer2 >= threshold)

terra::plot(layer2, col = c(adjustcolor(NA, alpha = 0), "darkgreen"), legend = FALSE)


img      <- terra::rast("/Users/mikaschubert/Documents/GitHub/citoMove/inst/examples/imgExample.jpg")
img_gray <- img[[1]] * 0.299 + img[[2]] * 0.587 + img[[3]] * 0.114
img_gray <- terra::flip(img_gray, direction = "vertical")
img_gray <- max(terra::values(img_gray), na.rm = TRUE) - img_gray
terra::ext(img_gray) <- terra::ext(-200, 200, -300, 300)  # gleicher Extent wie layer2

terra::plot(img_gray, col = colorRampPalette(c("white", "black"))(100), legend = FALSE)

numLayers <- terra::sprc(layer1, layer2)
imgLayers <- terra::sprc(img_gray)
layers <- terra::sprc(layer1, layer2, img_gray)

plotSpatRasterCollection(layers,
                         colours = c("darkblue", "darkgreen", "black"),
                         onlyoverlap = TRUE)


track1 <- simulateTrack(nSteps = 10000,
                        nChoiceSet = 500,
                        numericLayers = numLayers,
                        betas = c(2, -1),
                        imageLayers = imgLayers,
                        imageBetas = c(-3))


points(track1$x_[1], track1$y_[1], pch = 16, col = "red")
lines(track1$x_, track1$y_, col = "red")


#------------------------------------------------------


rasterList <- terra::as.list(layers)
xminAll <- max(sapply(rasterList, function(l) terra::ext(l)[1]))
xmaxAll <- min(sapply(rasterList, function(l) terra::ext(l)[2]))
yminAll <- max(sapply(rasterList, function(l) terra::ext(l)[3]))
ymaxAll <- min(sapply(rasterList, function(l) terra::ext(l)[4]))
extAll  <- terra::ext(xminAll, xmaxAll, yminAll, ymaxAll)
rasters <- lapply(rasterList, function(l) terra::crop(l, extAll))

layer1_cropped <- terra::crop(layer1, extAll)
r_min_l1 <- terra::global(layer1_cropped, "min", na.rm = TRUE)[1, 1]
r_max_l1 <- terra::global(layer1_cropped, "max", na.rm = TRUE)[1, 1]
layer1_scaled <- (layer1_cropped - r_min_l1) / (r_max_l1 - r_min_l1)

layer2_cropped <- terra::crop(layer2, extAll)
r_min_l2 <- terra::global(layer2_cropped, "min", na.rm = TRUE)[1, 1]
r_max_l2 <- terra::global(layer2_cropped, "max", na.rm = TRUE)[1, 1]
layer2_scaled <- (layer2_cropped - r_min_l2) / (r_max_l2 - r_min_l2)

layer3_cropped <- terra::crop(img_gray, extAll)
r_min <- terra::global(layer3_cropped, "min", na.rm = TRUE)[1, 1]
r_max <- terra::global(layer3_cropped, "max", na.rm = TRUE)[1, 1]
img_gray_scaled <- (layer3_cropped - r_min) / (r_max - r_min)

extractKernelValues_vec <- function(raster, xs, ys, kernelDim, kernelFun) {
  res_x   <- terra::res(raster)[1]
  res_y   <- terra::res(raster)[2]
  half    <- floor(kernelDim / 2)
  offsets <- expand.grid(dx = seq(-half, half), dy = seq(-half, half))
  n_pts   <- length(xs)
  n_off   <- nrow(offsets)
  all_x   <- rep(xs, each = n_off) + rep(offsets$dx, times = n_pts) * res_x
  all_y   <- rep(ys, each = n_off) + rep(offsets$dy, times = n_pts) * res_y
  vals_all <- terra::extract(raster, cbind(all_x, all_y))[, 1]
  vals_mat <- matrix(vals_all, nrow = n_pts, ncol = n_off, byrow = TRUE)
  apply(vals_mat, 1, function(row) {
    v <- row[!is.na(row)]
    if (length(v) == 0) NA_real_ else kernelFun(v)
  })
}

stp <- steps(track1)
obs_avail <- random_steps(stp, n_control = 500)
obs_avail$log_sl_ <- log(obs_avail$sl_)
obs_avail$cos_ta_ <- cos(obs_avail$ta_)
obs_avail$x_toro_ <- ((obs_avail$x2_ - extAll[1]) %% (extAll[2] - extAll[1])) + extAll[1]
obs_avail$y_toro_ <- ((obs_avail$y2_ - extAll[3]) %% (extAll[4] - extAll[3])) + extAll[3]
obs_avail$caseNum <- as.numeric(obs_avail$case_)

obs_avail$layer1 <- terra::extract(layer1_scaled, cbind(obs_avail$x_toro_, obs_avail$y_toro_))[, 1]

obs_avail$layer2 <- terra::extract(layer2_scaled, cbind(obs_avail$x_toro_, obs_avail$y_toro_))[, 1]

obs_avail$layer3 <- extractKernelValues_vec(
  raster    = img_gray_scaled,
  xs        = obs_avail$x_toro_,
  ys        = obs_avail$y_toro_,
  kernelDim = 3,
  kernelFun = mean)



m1 <- fit_issf(obs_avail,
               case_ ~ layer1 + layer2 + layer3 + sl_ + log_sl_ + cos_ta_ + strata(step_id_),
               model = TRUE)

summary(m1)


m2 <- dnn_ssf(data = obs_avail,
              formula = caseNum ~ layer1 + layer2 + layer3 + sl_ + ta_,
              optimizer = "adam",
              epochs = 20,
              plot = FALSE)

summary(m2)
