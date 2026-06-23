



# 1. Data of some steps of one deer (EPSG:4326):
data <- read.csv("/Users/mikaschubert/Desktop/Masterarbeit/Red Deer Data October 2023/Red Deer BFNP Oct 2023.csv")
animal_data <- data[data$animals_id == data$animals_id[1], ]
animal_data <- animal_data[1:16, ]

# 2. Creating amt track:
trk <- amt::make_track(
  tbl  = animal_data,
  .x   = longitude,
  .y   = latitude,
  crs  = sf::st_crs(4326))

# 3. Projecting amt track from EPSG:4326 to EPSG:25832:
trk_25832 <- amt::transform_coords(trk, crs_to = sf::st_crs(25832))

# 4. Transforming amt track to amt steps:
stps <- amt::steps(trk_25832)

# 5. Creating citoMove random steps:
rand_stps <- createRandomSteps(
  x          = stps,
  estimator  = "traditional",
  n_control  = 5)

# 6. Extracting image kernels (citoMove):
rand_stps_img <- extractStepImagekernels(
  x            = rand_stps,
  tifDirectory = "/Users/mikaschubert/Desktop/Masterarbeit/disk data/Orthoimages/True_Orthophotos/10cm",
  kernelRadius = 10,
  imageScaling = c(0,255))








# 7. Plotting amt track and image kernels:

plot(trk_25832$x_, trk_25832$y_, type = "b", pch = 16)

kernelRadius <- 10

for (i in seq_len(nrow(rand_stps_img))) {
  img <- rand_stps_img$`10cm_band2`[[i]][, , 1] # <- selecting band

  img_raster <- as.raster(img[nrow(img):1, ])
  x_center <- rand_stps_img$x2_[i]
  y_center <- rand_stps_img$y2_[i]

  rasterImage(
    img_raster,
    xleft   = x_center - kernelRadius,
    xright  = x_center + kernelRadius,
    ybottom = y_center - kernelRadius,
    ytop    = y_center + kernelRadius
  )
}

points(rand_stps_img$x2_, rand_stps_img$y2_, pch = 16, col = "red", cex = 0.8)
points(trk_25832$x_, trk_25832$y_, pch = 16, col = "green", cex = 0.8)
lines(trk_25832$x_, trk_25832$y_, col = "green")
