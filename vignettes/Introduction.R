## ----include = FALSE----------------------------------------------------------
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

