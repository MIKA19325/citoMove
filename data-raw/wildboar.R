## code to prepare `wildboar` dataset goes here
library(tidyverse)

dat <- readRDS("data-raw/wildboars.RDS")
wildboar <- dat |> select(id, res, fits) |>
  filter(res == 720) |>
  select(-res) |>
  unnest(cols = fits)

wildboar <- wildboar |> select(-c(x1_, y1_, y2_, x2_)) |>
  mutate(step_id_ = paste0(id, "-", step_id_))

usethis::use_data(wildboar, overwrite = TRUE)
