#' @title Fit a Deep Neural Network Step-Selection Function (dnn_ssf)
#' @description Fits a custom deep neural network using \code{\link{dnn}}.
#' @param data object of class `random_steps` from the `amt` package.
#'
#' @export
#'


dnn_ssf <- function(data, ...) {
  UseMethod("dnn_ssf")
}


#' DNN-SSF Method for Data Frames
#'
#' Fits a deep neural network step-selection model from a `data.frame`
#' containing movement steps, covariates, and case/control indicators.
#'
#' @inheritParams dnn_ssf
#' @param formula A model formula of the form `case_ ~ cov1 + cov2 + ...`
#'   specifying the model to be fit.
#' @param batchsize Batch size used during neural network training. Either
#'   `"auto"` (default) or a numeric value.
#' @inheritDotParams dnn_ssf
#'
#' @return A list of class `"dnn_ssf"` containing the fitted model and metadata.
#'
#' @example inst/examples/dnn_ssf-example.R
#'
#' @seealso [dnn_ssf()], [cito::dnn()]
#'
#' @export

dnn_ssf.random_steps <- function(data, formula, batchsize = "auto", ...){

  # Additional parameters
  n_control <- attr(data, "n_control")
  strata_size <- n_control + 1

  # Validate input
  # Check for no NA in the data
  if(!all(complete.cases(data[, all.vars(formula)]))) {
    message("Some observations contain missing data points.")
    data <-  data[complete.cases(data[, all.vars(formula)]), ]
  }

  # Check all steps are ok
  if(!all(table(data$step_id_) == strata_size)) {
    message(paste0("Not all strata contain ", strata_size,  " data points."))
    # Remove strata with
    data <- subset(data, ave(step_id_, step_id_, FUN = length) == strata_size)
  }

  if(is.numeric(batchsize)) {
    batchsize = batchsize * n_control
    message("Batchsize = ", batchsize)
  }

  if(batchsize == "auto") {
    n = 0.1 * nrow(data) + n_control / 2
    batchsize = n - (n %% n_control)
    message("Batchsize equals to 10% of data = ", batchsize)
  }

  custom_loss = function(pred, true) {
    Y = true[,1]
    P = pred[,1]
    P = P$reshape(list(nrow(pred)/strata_size, strata_size))
    prob = torch::nnf_softmax(P, dim = 2)
    loss = torch::distr_bernoulli(
      prob = prob)$log_prob(Y$reshape(
        list(nrow(pred)/strata_size, strata_size))
      )$negative()$mean()
    return(loss)
  }

  fit = cito::dnn(
    formula, data = dat_ssf,  shuffle = FALSE,
    loss = custom_loss, baseloss = FALSE, batchsize = batchsize, plot = FALSE,
    verbose = FALSE, ...)

  class(fit) = c("dnn_ssf", class(fit))

  return(fit)
}
