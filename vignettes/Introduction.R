## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(citoMove)

## -----------------------------------------------------------------------------
set.seed(123)
library(amt)
library(citoMove)
library(terra)

## -----------------------------------------------------------------------------
forest <- amt::get_sh_forest()
forest <- terra::subst(forest, 0, NA)
forest_dist <- distance(forest)
names(forest_dist) <- "forest_dist"
plot(forest_dist)

## -----------------------------------------------------------------------------
dat_ssf <- amt::deer |> 
  steps_by_burst() |> 
  random_steps() |> 
  extract_covariates(forest_dist) |> 
  time_of_day() |> 
  mutate(case_ = as.integer(case_))

## -----------------------------------------------------------------------------
model = dnn_ssf(case_ ~ forest_dist, data = dat_ssf, epoch = 30L, plot = T, verbose = FALSE)

## ---- message=F---------------------------------------------------------------
plot(model)

## -----------------------------------------------------------------------------
summary(model)

## -----------------------------------------------------------------------------
ALE(model)

