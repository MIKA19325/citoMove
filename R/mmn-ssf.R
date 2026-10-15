#' MMN-SSF Method for Multi-Modal Data
#'
#' Fits a multi-modal step-selection model, combining several input branches (for example a
#' convolutional branch over habitat rasters and a fully connected branch over step covariates)
#' in one conditional-logit likelihood.
#'
#' @param formula A model formula in the form expected by [cito::mmn()], e.g.
#'   `case ~ dnn(~., data = X_dnn) + cnn(X = X_cnn, architecture = arch)`.
#' @param dataList Named list holding the response and every data object the formula refers to.
#' @param density Density of the steps under the movement kernel that was used to sample the random
#'   steps. Enters the likelihood as an offset on the linear predictor, not as a weight.
#' @param batchsize Batch size (in percent of the data) used during neural network training.
#' @param n_control Number of pseudo absences.
#' @param ... Arguments passed on to [cito::mmn()].
#'
#' @return A list of class `"mmn_ssf"` containing the fitted model and metadata.
#'
#' @example inst/examples/mmn_ssf-example.R
#'
#' @seealso [cito::mmn()], [dnn_ssf()], [cnn_ssf()]
#'
#' @export

mmn_ssf <- function(formula, dataList, density = NULL, batchsize = 0.1, n_control, ...){

  if(is.null(n_control)) {
    stop("The number of pseudo absences is unknown, please provide this using the 'n_control' argument.")
  }
  n_control = n_control + 1
  strata_size <- n_control

  resp = as.character(formula[[2]])
  if(!resp %in% names(dataList)) stop("response '", resp, "' not found in dataList")
  n_rows = NROW(dataList[[resp]])

  ssf_check_strata(dataList[[resp]], strata_size, density)
  batchsize = ssf_batchsize(batchsize, n_rows, strata_size)

  # cito gained `weights` on mmn after 1.2; without it the sampling-density offset cannot be
  # delivered to the loss, and silently dropping it would reintroduce exactly the bias this fixes
  args = list(formula, dataList = dataList, shuffle = FALSE,
              loss = ssf_custom_loss(strata_size), baseloss = NULL, batchsize = batchsize, ...)
  if(!is.null(density)) {
    if(!"weights" %in% names(formals(cito::mmn)))
      stop("this version of cito (", utils::packageVersion("cito"),
           ") has no 'weights' argument on mmn(), so the sampling density cannot be applied. ",
           "Install a newer cito, or call mmn_ssf() without 'density'.")
    args$weights = density
  }
  fit = do.call(cito::mmn, args)

  class(fit) = c("mmn_ssf", class(fit))

  return(fit)
}
