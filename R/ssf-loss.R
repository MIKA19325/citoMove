# Conditional-logit loss shared by dnn_ssf, cnn_ssf and mmn_ssf. It lives in one place because the
# sampling-density correction below was previously duplicated and wrong in every copy.
#
# When the random steps are not drawn uniformly, the sampling density q enters McFadden's (1978)
# sampling-of-alternatives correction as an OFFSET on the linear predictor with its coefficient
# fixed at one, p(i) = exp(V_i - log q_i) / sum_j exp(V_j - log q_j), not as a weight on the
# likelihood. Added to the loss after the softmax it is constant in the parameters, so its gradient
# is zero and it changes nothing at all.
ssf_custom_loss = function(strata_size) {
  function(pred, true, weights) {
    Y = true[, 1]
    P = pred[, 1]
    if (!is.null(weights)) P = P - weights[, 1]$log()
    P = P$reshape(list(nrow(pred) / strata_size, strata_size))
    prob = torch::nnf_softmax(P, dim = 2)
    loss = torch::distr_bernoulli(prob = prob)$log_prob(
      Y$reshape(list(nrow(pred) / strata_size, strata_size)))$negative()
    loss$mean()
  }
}

# batchsize is a fraction of the data and has to come out a multiple of the stratum size, or the
# reshape above mixes rows from different decision points into one pseudo-stratum
ssf_batchsize = function(fraction, n_rows, strata_size) {
  n = fraction * n_rows + strata_size / 2
  n - (n %% strata_size)
}

# Validate the stratum structure before fitting. The loss reshapes rows to
# (n_strata, strata_size) and never reorders, so every assumption below is one the loss relies on
# and none of them errors on its own: a stratum with two cases, or rows that are not grouped by
# stratum, trains happily on a likelihood the user did not ask for.
ssf_check_strata = function(y, strata_size, density = NULL, strata = NULL) {
  n = length(y)
  if (n %% strata_size != 0)
    stop("the number of rows (", n, ") is not a multiple of the stratum size (", strata_size,
         "); check 'n_control'")
  cases = colSums(matrix(as.numeric(y), strata_size, n / strata_size))
  bad = which(cases != 1)
  if (length(bad))
    stop(length(bad), " of ", n / strata_size, " strata do not contain exactly one case ",
         "(first at stratum ", bad[1], ", ", cases[bad[1]], " cases). Either 'n_control' is wrong, ",
         "the rows are not grouped by stratum, or the case indicator is.")
  if (!is.null(strata)) {
    blocks = matrix(as.character(strata), strata_size, n / strata_size)
    if (any(apply(blocks, 2, function(b) length(unique(b))) != 1))
      stop("rows are not grouped by stratum; the loss reshapes them in the order given")
  }
  if (!is.null(density)) {
    d = as.numeric(density)
    # cito computes in float32, so anything under ~1e-38 underflows to log(0) = -Inf and the whole
    # fit comes back NaN without an error
    if (any(!is.finite(d)) || any(d <= 1e-30))
      stop("'density' must be finite and larger than 1e-30; it enters the likelihood as ",
           "log(density) and float32 underflows below that")
  }
  invisible(TRUE)
}
