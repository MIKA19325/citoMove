## code to prepare `wildboar` dataset goes here
library(tidyverse)

dat <- readRDS("data-raw/wildboars.RDS")
wildboar <- dat |> select(id, res, fits) |>
  filter(res == 720) |>
  select(-res) |>
  unnest(cols = fits)

wildboar <- wildboar |> select(-c(x1_, y1_, y2_, x2_)) |>
  mutate(step_id_ = paste0(id, "-", step_id_))

# urban:
# IZW 1 - female, subadult, 60 kg
# IZW 4 - female, adult, 76 kg
# IZW 5 - male, subadult, 50 kg
# IZW 9 - female, subadult, 50 kg
#
# Rural:
# IZW 2 - female, subadult, 58 kg
# IZW 6 - male, subadult, 60 kg
# IZW 7 - male, subadult, 58 kg
# IZW 8 - female, adult, NA

urb_rur <- c("1" = "urban", "4" = "urban", "5" = "urban",
             "9" = "urban","2" = "rural", "6" = "rural",
             "7" = "rural", "8" = "rural")

sex <- c("1" = "female", "4" = "female", "5" = "male",
         "9" = "female", "2" = "female", "6" = "male",
         "7" = "male", "8" = "female" )

joint_labels <- paste0(urb_rur, "_",sex, "_", c(1, 4, 5, 9, 2, 6, 7, 8))
names(joint_labels) <- names(urb_rur)

# df_sub = df[df$res == "720", ]
# randomSteps = 21
# n = 0.1000000 * nrow(df_sub) + randomSteps/2
# batchsize = n - (n%%randomSteps)
#
# df_sub$dtw = scale(df_sub$dtw)[,1]
# df_sub$imp = scale(df_sub$imp)[,1]
# df_sub$sl_ = scale(df_sub$sl_)[,1]
# df_sub$ta_ = scale(df_sub$ta_)[,1]
#
# shuffled_groups = sample(unique(df_sub$step_id_))
# df_sub = df_sub[order(factor(df_sub$step_id_, levels = shuffled_groups)), ]
# df_sub$indiv = droplevels(df_sub$indiv)
#
#


usethis::use_data(wildboar, overwrite = TRUE)
