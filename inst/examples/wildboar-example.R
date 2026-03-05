library(amt)
library(citoMove)
library(terra)

data(wildboar)
str(wildboar)

# create a new covariate: the distance to the next forest.

forest <- amt::get_sh_forest()
forest <- terra::subst(forest, 0, NA)
forest_dist <- distance(forest)
names(forest_dist) <- "forest_dist"
plot(forest_dist)

# Load the tracking data of a red deer, create random steps, extract covariate

data(deer)
dat_ssf <- deer |>
  steps_by_burst() |>
  random_steps() |>
  extract_covariates(forest_dist) |>
  time_of_day() |>
  mutate(case_ = as.integer(case_))


# Fit a deep neural network (dnn):

model = dnn_ssf(case_ ~ forest_dist, data = dat_ssf, epoch = 10L, plot = FALSE, verbose = FALSE)
summary(model)


