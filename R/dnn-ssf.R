#' DNN-SSF Method for Data Frames
#'
#' Fits a deep neural network step-selection model from a `data.frame`
#' containing movement steps, covariates, and case/control indicators.
#'
#' @param formula A model formula of the form `case_ ~ cov1 + cov2 + ...`
#'   specifying the model to be fit.
#' @param data Data.frame containing the true and random steps, and the covariates.
#' @param batchsize Batch size (in percent of the data) used during neural network training.
#' @param n_control Number of pseudo absences.
#' @param ... Arguments passed on to [cito::dnn()].
#'
#' @return A list of class `"dnn_ssf"` containing the fitted model and metadata.
#'
#' @example inst/examples/dnn_ssf-example.R
#'
#' @seealso [cito::dnn()]
#'
#' @export

dnn_ssf <- function(formula, data, batchsize = 0.1, n_control = NULL, ...){

  # Additional parameters # Aber das impliziert ja dass nur data.frames aus amt gefittet werden können?

  if(inherits(data, "random_steps")) n_control = attr(data, "n_control")

  if(is.null(n_control)) {
      stop("The number of pseudo absences is unknown, please provide this using the 'n_control' argument.")
  }
  n_control = n_control + 1
  strata_size <- n_control



  # Validate input
  # Check for no NA in the data
  if(!all(stats::complete.cases(data[, all.vars(formula)]))) {
    stop("Some observations contain missing data points.") # Do not run with missing data.
    data <-  data[stats::complete.cases(data[, all.vars(formula)]), ]
  }

  # Check all steps are ok
  step_id_ = NULL
  if(!all(table(data$step_id_) == strata_size)) {
    message(paste0("Not all strata contain ", strata_size,  " data points."))
    # Remove strata with
    data <- subset(data, stats::ave(step_id_, step_id_, FUN = length) == strata_size)
  }

  n = batchsize * nrow(data) + n_control / 2
  batchsize = n - (n %% n_control)

  density = data$density
  if(is.null(density)) density = matrix(1.0, nrow = nrow(data))

  custom_loss = function(pred, true, weights) {
    Y = true[,1]
    P = pred[,1]
    P = P$reshape(list(nrow(pred)/strata_size, strata_size))
    prob = torch::nnf_softmax(P, dim = 2)
    loss = torch::distr_bernoulli(
      prob = prob)$log_prob(Y$reshape(
        list(nrow(pred)/strata_size, strata_size))
      )$negative()#$mean()

    if(!is.null(weights)) loss = loss + weights$log()$negative()$reshape(list(nrow(pred)/strata_size, strata_size))
    loss = loss$mean()
    return(loss)
  }
  fit = cito::dnn(
    formula, data = data,  shuffle = FALSE,
    loss = custom_loss, baseloss = FALSE, batchsize = batchsize, weights = density, ...)

  class(fit) = c("dnn_ssf", class(fit))

  return(fit)
}
