#' @title DeepSSF
#' @description Fits a custom deep neural network using \code{\link{dnn}}.
#' @param formula an object of class "formula": a description of the model that should be fitted
#' @param data object of class citoMoveSSFData created by \code{\link{prepareSSFData}}
#'
#' @export
#' @example /inst/examples/fitDeepSSFHelp.R
#'
fitDeepSSF <- function(formula = NULL, data = NULL, batchsize = "auto", ...){

  if(class(data) != "citoMoveSSFData" & class(data) != "citoMoveSimulation"){
    stop("Data needs to be of class citoMoveSSFData")
  }

  dat_ssf = data$Data

  if(is.numeric(batchsize)) {
    batchsize = batchsize * data$StrataSteps
    message("Batchsize = ", batchsize)
  }

  if(batchsize == "auto") {
    n = 0.1 * nrow(data$Data) + data$StrataSteps/2
    batchsize = n - (n%%data$StrataSteps)
    message("Batchsize equals to 10% of data = ", batchsize)
  }


  custom_loss = function(pred, true) {
    Y = true[,1]
    P = pred[,1]
    P = P$reshape(list(nrow(pred)/strata_size, strata_size))
    prob = torch::nnf_softmax(P, dim = 2)
    loss = torch::distr_bernoulli(prob = prob)$log_prob(Y$reshape(list(nrow(pred)/strata_size, strata_size)))$negative()$mean()
    return(loss)
  }

  strata_size = data$StrataSteps

  fit = cito::dnn(formula, data = dat_ssf,  shuffle = FALSE, loss = custom_loss, baseloss = FALSE, batchsize = batchsize,...)

  class(fit) = c("citoMoveDeepSSF", class(fit))

  return(fit)
}


#plot.citoMoveSSMData
#summary.citoMoveSSMData
