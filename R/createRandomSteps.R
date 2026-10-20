#' Generate random steps with sampling densities for step selection functions
#'
#' For every observed step, draws `n_control` random (control) steps from a
#' movement kernel estimated from the observed step lengths and turning
#' angles, and stores the density of every step under that kernel. Three
#' estimators of the movement kernel are available: independent gamma and von
#' Mises distributions (`"traditional"`), a bivariate kernel density estimate
#' (`"density"`), and a random forest density surface sampled with MCMC
#' (`"density_advanced"`). The stored densities allow correcting the
#' likelihood of step selection models for non-uniform sampling of random
#' steps, e.g. in [dnn_ssf()] and [cnn_ssf()].
#'
#' @param x An `amt` object of class `steps_xy`, `steps_xyt`, `track_xy` or
#'   `track_xyt`. Tracks are converted to steps with [amt::steps()]. Steps
#'   with missing step length, turning angle or direction (e.g. the first step
#'   of a track) are removed.
#' @param estimator Character. Estimator of the movement kernel:
#'   `"density"` (default), `"traditional"` or `"density_advanced"`. See
#'   Details.
#' @param n_control Positive integer. Number of random steps per observed
#'   step.
#' @param rf_ntree Positive integer. Only used for
#'   `estimator = "density_advanced"`: number of trees of the random forest.
#' @param rf_n_bins Positive integer. Only used for
#'   `estimator = "density_advanced"`: number of bins per axis (step length
#'   and turning angle) of the grid on which the random forest is trained.
#' @param sl_transform Character. Only used for
#'   `estimator = "density_advanced"`: `"none"` (default) bins step lengths on
#'   their original scale, `"log1p"` bins them on the `log1p()` scale, which
#'   gives finer bins for short steps and suits strongly right-skewed step
#'   length distributions. A warning is issued if set for another estimator.
#' @param mcmc_burn_in Positive integer. Only used for
#'   `estimator = "density_advanced"`: number of burn-in iterations of the
#'   MCMC sampler.
#'
#' @return An object of class `random_steps_prop` (inheriting from
#'   `random_steps` and `steps_xy` or `steps_xyt`). Each observed step
#'   (`case_ = TRUE`) is followed by its `n_control` random steps
#'   (`case_ = FALSE`), all sharing the same `step_id_` (one stratum per
#'   observed step). The column `density` holds the density of each step's
#'   combination of step length (`sl_`) and turning angle (`ta_`, radians)
#'   under the estimated movement kernel. The attribute `n_control` stores
#'   the number of random steps per stratum.
#'
#'   For `"traditional"`, all columns returned by [amt::random_steps()] are
#'   kept. For `"density"` and `"density_advanced"`, the columns are
#'   `step_id_`, `case_`, `density`, `x1_`, `x2_`, `y1_`, `y2_`, `sl_`,
#'   `ta_` and, if present in `x`, `t1_`, `t2_` and `dt_`. For
#'   `"density_advanced"`, the MCMC object is attached as attribute
#'   `mcmc_out`, e.g. for [BayesianTools::tracePlot()].
#'
#' @details
#' **Estimators.**
#' - `"traditional"`: random steps are generated with [amt::random_steps()],
#'   which fits a gamma distribution to step lengths and a von Mises
#'   distribution to turning angles. `density` is the product of both fitted
#'   densities, i.e. step length and turning angle are assumed to be
#'   independent.
#' - `"density"`: a bivariate Gaussian kernel density estimate of step length
#'   and turning angle is fitted with [ks::kde()] (plug-in bandwidth,
#'   [ks::Hpi()]), and random steps are drawn from it with [ks::rkde()]
#'   (smoothed bootstrap). Draws with a step length of zero or below, or a
#'   turning angle outside -pi to pi, are discarded. Unlike the traditional
#'   approach, this captures correlations between step length and turning
#'   angle and multimodal distributions. Turning angles are treated as linear,
#'   not circular, so densities close to -pi and pi are slightly
#'   underestimated.
#' - `"density_advanced"`: a random forest is trained on binned counts of
#'   observed step lengths and turning angles, normalised by Monte Carlo
#'   integration, and sampled with MCMC (\pkg{BayesianTools}). Unlike the
#'   kernel density estimate, the surface is not pulled towards zero at the
#'   edges of the observed range. Densities are bounded below by `1e-10`, so
#'   that their logarithm is always finite.
#'
#' For both density-based estimators, step lengths are considered between 0
#' and 1.5 times the longest observed step.
#'
#' **Using the densities.** Random steps drawn from the movement kernel are not
#' uniformly distributed in space. Subtracting `log(density)` from the linear
#' predictor of each step before normalising within strata
#' (importance-sampling correction) accounts for this. [dnn_ssf()] reads the
#' column `density` automatically; for [cnn_ssf()], pass it as
#' `density = x$density`. To fit without the correction, remove the column
#' before calling [dnn_ssf()].
#'
#' **Internal helpers** (only used for `"density_advanced"`), defined at the
#' top of the function body:
#' - `fitDensitySurfaceRF()` bins the observed (step length, turning angle)
#'   pairs onto a grid and trains a random forest that predicts the bin
#'   counts.
#' - `evalDensitySurfaceRF()` evaluates the fitted (unnormalised) surface at
#'   arbitrary points, applying the Jacobian correction required for
#'   `sl_transform = "log1p"`.
#' - `estimateNormalizingConstantRF()` estimates, by Monte Carlo integration,
#'   the constant that turns the surface into a density integrating to 1.
#' - `bayesianToolsSampleDensitySurface()` draws new (step length, turning
#'   angle) pairs from the normalised surface with the `"DEzs"` sampler of
#'   [BayesianTools::runMCMC()].
#'
#' The movement kernel is estimated from all steps in `x` jointly, and
#' `step_id_` is renumbered consecutively. Results are random; use
#' [set.seed()] for reproducible random steps.
#'
#' @example inst/examples/dataprep-example.R
#'
#' @seealso [amt::random_steps()], [extractStepCovariates()],
#'   [extractStepImagekernels()], [dnn_ssf()], [cnn_ssf()]
#'
#' @author Mika Schubert
#' @export

createRandomSteps <- function(x,
                              estimator    = c("density", "traditional", "density_advanced"),
                              n_control    = 10,
                              rf_ntree     = 500,
                              rf_n_bins    = 30,
                              sl_transform = c("none", "log1p"),
                              mcmc_burn_in = 1000) {

  # Helper function: Bins the observed (sl, ta) pairs onto a 2D grid and
  # trains a random forest predicting the bin-count "landscape" over the
  # (sl, ta) space. Unlike a KDE, the surface is not forced to zero at the
  # edges but interpolated from neighbouring bins.
  #   sl, ta       - observed step lengths / turning angles (radians)
  #   sl_max       - upper bound of the sl grid
  #   n_bins_sl/ta - number of bins along each axis
  #   ntree        - number of trees in the random forest
  #   sl_transform - "log1p": equal-width bins on the log1p(sl) scale
  #                  (assumes a right-skewed sl distribution, as is typical
  #                  for movement data). "none": equal-width bins on the
  #                  original sl scale.
  # Returns: a randomForest object (with attribute "sl_transform" attached).
  fitDensitySurfaceRF <- function(sl, ta, sl_max,
                                  n_bins_sl = 30, n_bins_ta = 30,
                                  ntree = 500,
                                  sl_transform = c("log1p", "none")) {

    sl_transform <- match.arg(sl_transform)
    ta_breaks <- seq(-pi, pi, length.out = n_bins_ta + 1)
    ta_mid    <- (utils::head(ta_breaks, -1) + utils::tail(ta_breaks, -1)) / 2

    if (sl_transform == "log1p") {
      # log1p(sl) spreads out small sl differences and compresses large
      # ones; equal-width bins on this scale are therefore narrow near 0
      # and wide in the tail.
      sl_pred     <- log1p(sl)
      sl_max_pred <- log1p(sl_max)
      sl_breaks   <- seq(0, sl_max_pred, length.out = n_bins_sl + 1)
      sl_mid      <- (utils::head(sl_breaks, -1) + utils::tail(sl_breaks, -1)) / 2

      sl_bin <- cut(sl_pred, breaks = sl_breaks, include.lowest = TRUE)
      ta_bin <- cut(ta, breaks = ta_breaks, include.lowest = TRUE)
      count_table <- table(factor(sl_bin, levels = levels(sl_bin)),
                           factor(ta_bin, levels = levels(ta_bin)))

      # Bins are equal-width on this (transformed) scale, so raw counts
      # are already proportional to density; no bin-width division needed.
      grid_df   <- expand.grid(sl_pred = sl_mid, ta = ta_mid)
      grid_df$y <- as.vector(count_table)

    } else {
      # "none": equal-width bins on the original sl scale.
      sl_breaks <- seq(0, sl_max, length.out = n_bins_sl + 1)
      sl_mid    <- (utils::head(sl_breaks, -1) + utils::tail(sl_breaks, -1)) / 2

      sl_bin <- cut(sl, breaks = sl_breaks, include.lowest = TRUE)
      ta_bin <- cut(ta, breaks = ta_breaks, include.lowest = TRUE)
      count_table <- table(factor(sl_bin, levels = levels(sl_bin)),
                           factor(ta_bin, levels = levels(ta_bin)))

      grid_df   <- expand.grid(sl_pred = sl_mid, ta = ta_mid)
      grid_df$y <- as.vector(count_table)
    }

    rf_fit <- randomForest::randomForest(y ~ sl_pred + ta,
                                         data  = grid_df,
                                         ntree = ntree,
                                         mtry  = 2)

    # Attach the transform as an attribute so evalDensitySurfaceRF() later
    # automatically knows how to treat sl before predict() - no need to
    # track/pass this separately.
    attr(rf_fit, "sl_transform") <- sl_transform
    rf_fit
  }


  # Helper function: Evaluates the (unnormalized) fitted RF density surface
  # at arbitrary (sl, ta) points, applying the Jacobian correction needed
  # when the RF was trained on the log1p(sl) scale.
  #   sl, ta - points at which to evaluate
  #   rf_fit - randomForest object from fitDensitySurfaceRF()
  # Returns: numeric vector of predicted (unnormalized) density values.
  evalDensitySurfaceRF <- function(sl, ta, rf_fit) {
    sl_transform <- attr(rf_fit, "sl_transform")

    if (sl_transform == "log1p") {
      # RF was trained on log1p(sl) -> apply the same transform before predict().
      sl_pred <- log1p(sl)
      pred    <- stats::predict(rf_fit, newdata = data.frame(sl_pred = sl_pred, ta = ta))

      # Jacobian correction: the RF prediction is a density on the
      # log1p(sl) scale, not the original sl scale. Converting a density
      # between scales requires multiplying by the derivative of the
      # transform: d(log1p(sl))/d(sl) = 1 / (1 + sl). Without this, the
      # tail (large sl) density would be systematically overestimated.
      jacobian <- 1 / (1 + sl)
      dens     <- pred * jacobian

    } else {
      # "none": RF was trained directly on the original sl scale with
      # constant-width bins, so no back-transform / Jacobian is needed.
      pred <- stats::predict(rf_fit, newdata = data.frame(sl_pred = sl, ta = ta))
      dens <- pred
    }

    # pmax(..., 0): clip tiny negative values from numerical rounding
    # (a density cannot be negative).
    pmax(dens, 0)
  }

  # Helper function: Estimates the normalizing constant Z via Monte Carlo
  # integration, so the raw RF surface can be turned into a proper density
  # (one that integrates to 1). Draws n_mc uniform points over the
  # [0, sl_max] x [-pi, pi] rectangle, evaluates the surface there, and
  # multiplies the mean value by the rectangle's area.
  #   rf_fit - randomForest object
  #   sl_max - upper bound of the sl range
  #   n_mc   - number of Monte Carlo samples
  # Returns: numeric(1), the estimated normalizing constant Z.
  estimateNormalizingConstantRF <- function(rf_fit, sl_max, n_mc = 20000) {
    mc_sl   <- stats::runif(n_mc, 0, sl_max)
    mc_ta   <- stats::runif(n_mc, -pi, pi)
    mc_vals <- evalDensitySurfaceRF(mc_sl, mc_ta, rf_fit)

    domain_area <- sl_max * 2 * pi
    mean(mc_vals) * domain_area
  }

  # Helper function: Draws new (sl, ta) pairs from the normalized RF
  # surface via MCMC (BayesianTools). BayesianTools expects a target
  # function returning a LOG-density (standard for MCMC, for numerical
  # stability). Uses the "DEzs" sampler (Differential-Evolution MCMC with
  # multiple chains), which is more robust than plain Metropolis-Hastings
  # for a piecewise-constant (step-like) RF surface.
  #   n          - desired number of samples (after burn-in)
  #   rf_fit     - randomForest object
  #   sl_max     - upper bound of the sl range
  #   burn_in    - number of initial iterations to discard
  #   iterations - total MCMC iterations; if NULL, set to burn_in + n * 5
  # Returns: a list with
  #   samples  - matrix with columns "sl" and "ta"
  #   mcmc_out - the full BayesianTools MCMC object (e.g. for
  #              BayesianTools::tracePlot() or gelmanDiagnostics())
  bayesianToolsSampleDensitySurface <- function(n, rf_fit, sl_max,
                                                burn_in    = 1000,
                                                iterations = NULL) {

    log_density_fn <- function(par) {
      sl_val <- par[1]
      ta_val <- par[2]

      # Hard bounds: proposals outside the valid range get probability 0
      # (log(0) = -Inf) and are rejected
      if (sl_val < 0 || sl_val > sl_max || ta_val < -pi || ta_val > pi) {
        return(-Inf)
      }

      dens <- evalDensitySurfaceRF(sl_val, ta_val, rf_fit)
      # + 1e-10 avoids log(0) = -Inf where the RF predicts exactly 0.
      log(dens + 1e-10)
    }

    bayesianSetup <- BayesianTools::createBayesianSetup(
      likelihood = log_density_fn,
      lower      = c(0, -pi),
      upper      = c(sl_max, pi)
    )

    if (is.null(iterations)) iterations <- burn_in + n * 5

    settings <- list(iterations = iterations, burnin = burn_in, message = FALSE)

    mcmc_out <- BayesianTools::runMCMC(bayesianSetup = bayesianSetup,
                                       sampler       = "DEzs",
                                       settings      = settings)

    samples_matrix <- BayesianTools::getSample(mcmc_out)
    colnames(samples_matrix) <- c("sl", "ta")

    # If more samples than needed:
    if (nrow(samples_matrix) > n) {
      keep           <- sample(seq_len(nrow(samples_matrix)), n)
      samples_matrix <- samples_matrix[keep, , drop = FALSE]
    }

    # If fewer samples than needed:
    if (nrow(samples_matrix) < n) {
      warning(
        "BayesianTools MCMC returned only ", nrow(samples_matrix),
        " samples after burn-in, but ", n, " were requested. ",
        "Filling the gap by resampling with replacement; consider increasing ",
        "`iterations` if this happens often.",
        call. = FALSE
      )
      extra_idx <- sample(seq_len(nrow(samples_matrix)),
                          size    = n - nrow(samples_matrix),
                          replace = TRUE)
      samples_matrix <- rbind(samples_matrix, samples_matrix[extra_idx, , drop = FALSE])
    }

    list(samples = samples_matrix, mcmc_out = mcmc_out)
  }


  # Validation:
  sl_transform_was_set <- !missing(sl_transform)
  sl_transform <- match.arg(sl_transform)

  if (!inherits(x, c("steps_xy", "steps_xyt", "track_xy", "track_xyt"))) {
    stop("`x` must be an amt `steps_xy`, `steps_xyt`, `track_xy`, or `track_xyt` object.")
  }
  if (!is.numeric(n_control) || length(n_control) != 1L ||
      n_control != floor(n_control) || n_control < 1L) {
    stop("`n_control` must be a single positive integer.")
  }
  n_control <- as.integer(n_control)

  if (inherits(x, c("track_xy", "track_xyt")) &&
      !inherits(x, c("steps_xy", "steps_xyt"))) {
    x <- amt::steps(x)
  }
  x$step_id_ <- seq_len(nrow(x))
  estimator  <- match.arg(estimator)

  # sl_transform only has an effect for "density_advanced"
  if (sl_transform_was_set && estimator != "density_advanced") {
    warning(
      "`sl_transform` is only used for `estimator = \"density_advanced\"` ",
      "and is ignored here, since `estimator = \"", estimator, "\"` was chosen.",
      call. = FALSE
    )
  }


  # Branch I: estimator = "traditional"
  if (estimator == "traditional") {
    valid   <- !is.na(x$ta_) & !is.na(x$direction_p) & !is.na(x$sl_)
    x       <- x[valid, ]
    out     <- amt::random_steps(x, n_control = n_control)
    sl_fit  <- attr(out, "sl_")
    ta_fit  <- attr(out, "ta_")
    sl_prob <- stats::dgamma(out$sl_,
                             shape = sl_fit$params$shape,
                             scale = sl_fit$params$scale)
    ta_prob <- as.numeric(circular::dvonmises(
      circular::circular(out$ta_),
      mu    = ta_fit$params$mu,
      kappa = ta_fit$params$kappa))
    out$density <- sl_prob * ta_prob
    class(out) <- c("random_steps_prop", class(out))
    return(out)
  }

  # Preparations for the density-based estimators
  valid   <- !is.na(x$ta_) & !is.na(x$direction_p) & !is.na(x$sl_)
  x_valid <- x[valid, ]
  sl_max  <- max(x_valid$sl_) * 1.5
  n_obs   <- nrow(x_valid)


  # Branch II: estimator = "density" (KDE)
  if (estimator == "density") {

    obs_movement <- cbind(sl = x_valid$sl_, ta = x_valid$ta_)
    H_hat   <- ks::Hpi(obs_movement)
    kde_fit <- ks::kde(obs_movement, H = H_hat, xmin = c(0, -pi), xmax = c(sl_max, pi))

    draw_kde <- function(n, kde_obj) {
      candidates <- ks::rkde(n * 2, fhat = kde_obj)
      keep <- candidates[candidates[, "sl"] > 0 &
                           candidates[, "ta"] >= -pi &
                           candidates[, "ta"] <= pi, , drop = FALSE]
      if (nrow(keep) < n) stop("Too many drawn samples out of range.")
      keep[seq_len(n), ]
    }

    n_total      <- n_obs * n_control
    random_draws <- draw_kde(n_total, kde_fit)
    sl_random    <- random_draws[, "sl"]
    ta_random    <- random_draws[, "ta"]

    obs_idx   <- rep(seq_len(n_obs), each = n_control)
    abs_angle <- x_valid$direction_p[obs_idx] + ta_random
    x2_random <- x_valid$x1_[obs_idx] + sl_random * cos(abs_angle)
    y2_random <- x_valid$y1_[obs_idx] + sl_random * sin(abs_angle)

    random_tbl          <- x_valid[obs_idx, ]
    random_tbl$x2_      <- x2_random
    random_tbl$y2_      <- y2_random
    random_tbl$sl_      <- sl_random
    random_tbl$ta_      <- ta_random
    random_tbl$case_    <- FALSE
    random_tbl$step_id_ <- rep(x_valid$step_id_, each = n_control)
    random_tbl$density  <- stats::predict(kde_fit, x = cbind(sl_random, ta_random))

    obs_tbl         <- x_valid
    obs_tbl$case_   <- TRUE
    obs_tbl$density <- stats::predict(kde_fit, x = cbind(x_valid$sl_, x_valid$ta_))

    core_cols <- c("step_id_", "case_", "density", "x1_", "x2_", "y1_", "y2_", "sl_", "ta_")
    time_cols <- intersect(c("t1_", "t2_", "dt_"), names(x_valid))
    keep_cols <- c(core_cols, time_cols)

    out <- rbind(obs_tbl[, keep_cols], random_tbl[, keep_cols])
    out <- out[order(out$step_id_, !out$case_), ]

    base_steps_class <- if (inherits(x, "steps_xyt")) "steps_xyt" else "steps_xy"
    class(out) <- c(
      "random_steps_prop",
      "random_steps",
      base_steps_class,
      setdiff(class(out), c("random_steps_prop", "random_steps",
                            "steps_xyt", "steps_xy"))
    )
    attr(out, "n_control") <- n_control
    return(out)
  }


  # Branch III: estimator = "density_advanced" (Random Forest + BayesianTools)
  if (estimator == "density_advanced") {

    # 1. Fit the density landscape
    rf_fit <- fitDensitySurfaceRF(x_valid$sl_, x_valid$ta_, sl_max,
                                  n_bins_sl    = rf_n_bins,
                                  n_bins_ta    = rf_n_bins,
                                  ntree        = rf_ntree,
                                  sl_transform = sl_transform)

    # 2. Estimate the normalizing constant
    Z <- estimateNormalizingConstantRF(rf_fit, sl_max)

    # 3. Draw random steps from the normalized surface
    n_total   <- n_obs * n_control
    mh_result <- bayesianToolsSampleDensitySurface(n_total, rf_fit, sl_max,
                                                   burn_in = mcmc_burn_in)
    sl_random <- mh_result$samples[, "sl"]
    ta_random <- mh_result$samples[, "ta"]

    # 4. Reconstruct end coordinates
    obs_idx   <- rep(seq_len(n_obs), each = n_control)
    abs_angle <- x_valid$direction_p[obs_idx] + ta_random
    x2_random <- x_valid$x1_[obs_idx] + sl_random * cos(abs_angle)
    y2_random <- x_valid$y1_[obs_idx] + sl_random * sin(abs_angle)

    random_tbl          <- x_valid[obs_idx, ]
    random_tbl$x2_      <- x2_random
    random_tbl$y2_      <- y2_random
    random_tbl$sl_      <- sl_random
    random_tbl$ta_      <- ta_random
    random_tbl$case_    <- FALSE
    random_tbl$step_id_ <- rep(x_valid$step_id_, each = n_control)
    random_tbl$density  <- pmax(evalDensitySurfaceRF(sl_random, ta_random, rf_fit) / Z, 1e-10)

    obs_tbl         <- x_valid
    obs_tbl$case_   <- TRUE
    obs_tbl$density <- pmax(evalDensitySurfaceRF(x_valid$sl_, x_valid$ta_, rf_fit) / Z, 1e-10)

    core_cols <- c("step_id_", "case_", "density", "x1_", "x2_", "y1_", "y2_", "sl_", "ta_")
    time_cols <- intersect(c("t1_", "t2_", "dt_"), names(x_valid))
    keep_cols <- c(core_cols, time_cols)

    out <- rbind(obs_tbl[, keep_cols], random_tbl[, keep_cols])
    out <- out[order(out$step_id_, !out$case_), ]

    base_steps_class <- if (inherits(x, "steps_xyt")) "steps_xyt" else "steps_xy"
    class(out) <- c(
      "random_steps_prop",
      "random_steps",
      base_steps_class,
      setdiff(class(out), c("random_steps_prop", "random_steps",
                            "steps_xyt", "steps_xy"))
    )
    attr(out, "n_control") <- n_control

    # Attach the MCMC object as an attribute for later diagnostics
    attr(out, "mcmc_out") <- mh_result$mcmc_out

    return(out)
  }
}
