#' CNN-SSF Method for Data Frames
#'
#' Fits a convolutional neural network step-selection model from a `data.frame`
#' containing movement steps, covariates, and case/control indicators.
#'
#' @param X Array of images describing the habitat
#' @param Y Vectors of steps and pseudo steps (1 and 0)
#' @param density density of steps given the movement kenrnel that was used to sample the random steps
#' @param batchsize Batch size (in percent of the data) used during neural network training.
#' @param n_control Number of pseudo/random steps
#' @param ... Arguments passed on to [cito::cnn()]
#'
#' @return A list of class `"cnn_ssf"` containing the fitted model and metadata.
#'
#' @example inst/examples/cnn_ssf-example.R
#'
#' @seealso [cito::cnn()]
#'
#' @export

cnn_ssf <- function(X, Y, density = NULL, batchsize = 0.1, n_control, ...){

  # Additional parameters # Aber das impliziert ja dass nur data.frames aus amt gefittet werden können?

  n_control = n_control + 1
  strata_size <- n_control

  if(is.vector(Y)) Y = matrix(Y, ncol = 1L)

  n = batchsize * nrow(Y) + n_control / 2
  batchsize = n - (n %% n_control)

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
  fit = cito::cnn(
    X = X, Y = Y,  shuffle = FALSE, weights = density,
    loss = custom_loss, baseloss = FALSE, batchsize = batchsize, ...)

  class(fit) = c("cnn_ssf", class(fit))

  return(fit)
}
