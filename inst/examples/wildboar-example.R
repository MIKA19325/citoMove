library(amt)
library(citoMove)
library(terra)

data(wildboar)
str(wildboar)

# First, we create a new raster layer with the distance to the next forest.

forest <- amt::get_sh_forest()
forest <- terra::subst(forest, 0, NA)
forest_dist <- distance(forest)
names(forest_dist) <- "forest_dist"
plot(forest_dist)

# Next, we
# 1) load the tracking data of a red deer
# 2) create random steps
# 3) extract covariate values for random and real steps from the raster layer

data(deer)
dat_ssf <- deer |>
  steps_by_burst() |>
  random_steps() |>
  extract_covariates(forest_dist) |>
  time_of_day() |>
  mutate(case_ = as.integer(case_))

# The results are provided as a data.frame / tibble

# With this, we can fit a SSD based on a deep neural network, using the
# dnn_ssf function. For help on parameter, see ?cito::dnn

model = dnn_ssf(case_ ~ forest_dist, data = dat_ssf, epoch = 10L, plot = FALSE, verbose = FALSE)
summary(model)


