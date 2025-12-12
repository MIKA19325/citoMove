#' Prepocessed data of tracked wild boards
#'
#' A dataset containing observed and random steps annotated environmental covariates
#' for GPS-tracked wild boars. The GPS data recorded in a study
#' of urban and rural wild boar movement patterns in the Berlin area.
#'
#' @format A tibble with 68,565 rows and 11 variables:
#' \describe{
#'   \item{id}{Factor identifying each wild boar (8 individuals).}
#'   \item{sl_}{Numeric Step length.}
#'   \item{ta_}{Numeric Turning angle between consecutive steps (radians).}
#'   \item{t1_}{POSIXct Start timestamp of the step.}
#'   \item{t2_}{POSIXct End timestamp of the step.}
#'   \item{dt_}{Difftime (hours). Time interval between `t1_` and `t2_` (all equal to 12 hours).}
#'   \item{case_}{Logical. `TRUE` indicates an observed step; `FALSE` indicates a control (available) step.}
#'   \item{step_id_}{Character. Identifier linking observed steps to their matched control steps.}
#'   \item{dtw}{Numeric. Distance to water.}
#'   \item{imp}{Numeric. Environmental or model-based importance score.}
#'   \item{hab}{Numeric. Habitat type or indicator variable.}
#' }
#'
#' @source
#' Stillfried, M., Gras, P., Börner, K., Göritz, F., Painer, J., Röllig, K.,
#' Wenzler, M., Hofer, H., Ortmann, S., & Kramer-Schadt, S. (2017).
#' *Secrets of Success in a Landscape of Fear: Urban Wild Boar Adjust Risk
#' Perception and Tolerate Disturbance*. *Frontiers in Ecology and Evolution*,
#' 5:157. https://doi.org/10.3389/fevo.2017.00157. :contentReference[oaicite:3]{index=3}
#'
#' @examples
#' data(wildboar)
#' dplyr::glimpse(wildboar)
"wildboar"
