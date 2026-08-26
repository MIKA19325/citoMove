#' Create Random Steps for Step Selection Functions
#'
#' Adds random (control) steps to a track or steps object, using either
#' a 2D kernel density estimator (via \pkg{ks}) fitted on step length
#' and turning angle, or the traditional approach (gamma + von Mises distributions).
#'
#' @param x An \code{amt} object of class \code{steps_xy} or \code{track_xy}.
#' @param estimator character(1). Either \code{"density"} (2D KDE + smoothened
#'   bootstrap via \pkg{ks}), \code{"traditional"} (gamma step-length + von Mises
#'   turning-angle) or \code{"density_advanced"} (2D density trough Random Forest
#'   + MCMC). Default: "density".
#' @param n_control positive integer. Number of random steps per observed step.
#'   Default: 10.
#' @param rf_ntree integer(1). Nur fuer \code{estimator = "density_advanced"}:
#'   Anzahl Baeume im Random Forest. Default: 500.
#' @param rf_n_bins integer(1). Nur fuer \code{estimator = "density_advanced"}:
#'   Anzahl Bins pro Achse fuer das Trainingsgitter. Default: 30.
#' @param sl_transform character(1). Nur fuer \code{estimator = "density_advanced"}:
#'   Wie die sl-Achse vor dem Binning behandelt wird - \code{"none"} (Default)
#'   oder \code{"log1p"} (passend fuer stark rechtsschiefe sl-Verteilungen)
#'   (gleich breite Bins in der Originalskala). Siehe den internen Helfer
#'   \code{fitDensitySurfaceRF()}, der weiter unten in dieser Funktion definiert ist.
#' @param mcmc_burn_in integer(1). Nur fuer \code{estimator = "density_advanced"}:
#'   Burn-in-Iterationen der MCMC-Kette. Default: 1000.
#'
#'
#' @return When \code{estimator = "density"}, an object of class \code{"random_steps_kde"}
#' with \code{n_control} random alternatives per observed step.
#' When \code{estimator = "traditional"}, an object of class \code{"random_steps"}.
#'
#'
#' @details
#' When \code{estimator = "density"}, step lengths (SL) and turning
#' angles (TA, in radians) are extracted from all observed steps, a
#' bivariate KDE is fitted and random steps are drawn.
#' When \code{estimator = "traditional"}, the function \code{\link[amt]{random_steps}}
#' of the amt package is used.
#'
#' When \code{estimator = "density_advanced"}, four internal helper routines
#' (not exported, and only used inside this function) implement the
#' Random-Forest + MCMC approach:
#' \itemize{
#'   \item \code{fitDensitySurfaceRF()} bins the observed (sl, ta) pairs onto
#'     a grid and trains a random forest that predicts the "landscape" of
#'     bin counts over the (sl, ta) space. Unlike a classic KDE, the surface
#'     is not artificially forced to zero at the edges but interpolated from
#'     neighbouring bins.
#'   \item \code{evalDensitySurfaceRF()} evaluates that fitted random forest
#'     (unnormalized density) at arbitrary (sl, ta) points, applying the
#'     Jacobian correction needed when \code{sl_transform = "log1p"} was used.
#'   \item \code{estimateNormalizingConstantRF()} estimates, via Monte Carlo
#'     integration, the constant needed to turn the random forest's raw
#'     predictions into a proper probability density (one that integrates to 1).
#'   \item \code{bayesianToolsSampleDensitySurface()} draws new (sl, ta) pairs
#'     from that normalized surface using MCMC sampling (\pkg{BayesianTools}).
#' }
#' These four helpers are defined directly at the top of \code{createRandomSteps()}
#' since they are never needed outside of it.
#'
#' @example inst/examples/dataprep-example.R
#'
#' @export
#' @author Mika Schubert

createRandomSteps <- function(x,
                              estimator    = c("density", "traditional", "density_advanced"),
                              n_control    = 10,
                              rf_ntree     = 500,
                              rf_n_bins    = 30,
                              sl_transform = c("none", "log1p"),
                              mcmc_burn_in = 1000) {

  # Internal helper functions ---------------------------------------------
  # All four helpers below are only ever needed for estimator =
  # "density_advanced" (further down in this function) and nowhere else in
  # the package, so they are defined here as local functions instead of
  # being separate top-level functions. Being defined inside
  # createRandomSteps() means: they only exist while createRandomSteps() is
  # running, and bayesianToolsSampleDensitySurface() can call
  # evalDensitySurfaceRF() directly (as a "sibling" function defined in the
  # same enclosing function) without it having to be passed in as an argument.

  # Fit a 2D density surface using a random forest.
  #
  # Bins die beobachteten (sl, ta)-Paare in ein Gitter und trainiert einen
  # Random Forest, der die Haeufigkeits-"Landschaft" ueber dem (sl, ta)-Raum
  # vorhersagt. Im Gegensatz zur KDE wird die Flaeche an den Raendern nicht
  # kuenstlich auf 0 gezwungen, sondern aus den Nachbar-Bins interpoliert.
  #
  #   sl, ta       - beobachtete Step Lengths / Turning Angles (Radiant)
  #   sl_max       - Obergrenze des sl-Gitters
  #   n_bins_sl/ta - Anzahl Bins entlang der jeweiligen Achse
  #   ntree        - Anzahl Baeume im Random Forest
  #   sl_transform - "log1p": Bin-Grenzen gleich breit in der log1p(sl)-Skala.
  #                  Macht implizit die Annahme einer rechtsschiefen
  #                  sl-Verteilung (typisch fuer Bewegungsdaten, vgl. die
  #                  klassische Gamma-Verteilung fuer Step Lengths). Sehr
  #                  effektiv, wenn diese Annahme zutrifft.
  #                  "none": gleich breite Bins in der Original-sl-Skala
  #                  (einfachstes Verhalten - anfaellig fuer das eingangs
  #                  beschriebene Problem bei schiefen Verteilungen).
  #
  # Rueckgabe: ein randomForest-Objekt.
  fitDensitySurfaceRF <- function(sl, ta, sl_max,
                                  n_bins_sl = 30, n_bins_ta = 30,
                                  ntree = 500,
                                  sl_transform = c("log1p", "none")) {

    sl_transform <- match.arg(sl_transform)
    ta_breaks <- seq(-pi, pi, length.out = n_bins_ta + 1)
    ta_mid    <- (head(ta_breaks, -1) + tail(ta_breaks, -1)) / 2

    if (sl_transform == "log1p") {
      # --- log1p-Transformation: gleich breite Bins in der log1p(sl)-Skala ---
      # log1p(sl) = log(1 + sl): spreizt kleine sl-Unterschiede (z.B. 5m vs.
      # 20m), staucht grosse (z.B. 1000m vs. 1200m). Gleich breite Bins in
      # dieser Skala entsprechen damit ungleich breiten, aber inhaltlich
      # sinnvollen Bins im Original: schmal nahe 0, breit im Tail.
      sl_pred    <- log1p(sl)
      sl_max_pred <- log1p(sl_max)
      sl_breaks  <- seq(0, sl_max_pred, length.out = n_bins_sl + 1)
      sl_mid     <- (head(sl_breaks, -1) + tail(sl_breaks, -1)) / 2

      sl_bin <- cut(sl_pred, breaks = sl_breaks, include.lowest = TRUE)
      ta_bin <- cut(ta, breaks = ta_breaks, include.lowest = TRUE)
      count_table <- table(factor(sl_bin, levels = levels(sl_bin)),
                           factor(ta_bin, levels = levels(ta_bin)))

      # Bins sind in dieser (transformierten) Skala gleich breit -> rohe
      # Zaehldaten sind direkt proportional zur Dichte in dieser Skala.
      # Keine Division durch Bin-Breite notwendig.
      grid_df <- expand.grid(sl_pred = sl_mid, ta = ta_mid)
      grid_df$y <- as.vector(count_table)

    } else {
      # --- "none": gleich breite Bins in der Original-sl-Skala ---
      sl_breaks <- seq(0, sl_max, length.out = n_bins_sl + 1)
      sl_mid    <- (head(sl_breaks, -1) + tail(sl_breaks, -1)) / 2

      sl_bin <- cut(sl, breaks = sl_breaks, include.lowest = TRUE)
      ta_bin <- cut(ta, breaks = ta_breaks, include.lowest = TRUE)
      count_table <- table(factor(sl_bin, levels = levels(sl_bin)),
                           factor(ta_bin, levels = levels(ta_bin)))

      # Bins sind hier ebenfalls gleich breit (konstante Breite ueber den
      # gesamten sl-Bereich) -> rohe Zaehldaten sind direkt proportional zur
      # Dichte. Keine Division durch Bin-Breite notwendig.
      grid_df <- expand.grid(sl_pred = sl_mid, ta = ta_mid)
      grid_df$y <- as.vector(count_table)
    }

    # Random Forest fitten: y (Zaehldaten) ~ sl_pred + ta.
    rf_fit <- randomForest::randomForest(y ~ sl_pred + ta,
                                         data  = grid_df,
                                         ntree = ntree,
                                         mtry = 2)

    # Transformation als Attribut anhaengen, damit evalDensitySurfaceRF()
    # spaeter automatisch weiss, wie sl vor dem predict() behandelt werden
    # muss - ohne dass man sich das separat merken oder weiterreichen muss.
    attr(rf_fit, "sl_transform") <- sl_transform
    rf_fit
  }


  # Evaluate the (unnormalized) RF density surface at given points.
  #   sl, ta  - Punkte, an denen ausgewertet werden soll
  #   rf_fit  - randomForest-Objekt aus fitDensitySurfaceRF()
  # Rueckgabe: numeric vector mit vorhergesagten (unnormierten) Dichtewerten.
  evalDensitySurfaceRF <- function(sl, ta, rf_fit) {
    # sl_transform wurde beim Fitten als Attribut gespeichert (siehe
    # fitDensitySurfaceRF()) - dadurch muss man sich diese Information nicht
    # separat merken und an jede Stelle weiterreichen, an der das Modell
    # ausgewertet wird.
    sl_transform <- attr(rf_fit, "sl_transform")

    if (sl_transform == "log1p") {
      # Der RF wurde auf log1p(sl) trainiert -> dieselbe Transformation muss
      # vor dem predict() angewendet werden.
      sl_pred <- log1p(sl)
      pred <- stats::predict(rf_fit, newdata = data.frame(sl_pred = sl_pred, ta = ta))

      # JACOBIAN-KORREKTUR: Die RF-Vorhersage ist eine Dichte in der log1p(sl)-
      # Skala, nicht in der Original-sl-Skala. Beim Zuruecktransformieren
      # einer Dichte von einer Skala in eine andere muss man mit der
      # Ableitung der Transformation multiplizieren:
      #   d(log1p(sl))/d(sl) = 1 / (1 + sl)
      # Ohne diese Korrektur wuerde die Dichte im Tail (grosse sl)
      # systematisch ueberschaetzt.
      jacobian <- 1 / (1 + sl)
      dens <- pred * jacobian

    } else {
      # "none": der RF wurde direkt auf der Original-sl-Skala mit rohen
      # Zaehldaten als Zielvariable trainiert (siehe fitDensitySurfaceRF()) -
      # die Bins sind dort konstant breit, daher ist keine Rueck-
      # Transformation oder Jacobian-Korrektur notwendig.
      pred <- stats::predict(rf_fit, newdata = data.frame(sl_pred = sl, ta = ta))
      dens <- pred
    }

    # pmax(..., 0): numerische Rundungsfehler koennen winzige negative Werte
    # erzeugen, die wir hier auf 0 abschneiden (eine Dichte kann nicht negativ sein).
    pmax(dens, 0)
  }


  # Estimate the normalizing constant via Monte Carlo integration.
  #
  # predict() auf einem randomForest liefert keine echte Dichte (das Integral
  # ueber den gesamten Raum ist im Allgemeinen ungleich 1). Um daraus eine
  # gueltige Wahrscheinlichkeitsdichte zu machen, schaetzen wir das Integral
  # der Flaeche numerisch (Monte-Carlo-Integration) und teilen spaeter durch
  # diesen Wert.
  #
  # Idee: Integral ueber ein Rechteck = (Flaeche des Rechtecks) * (Mittelwert
  # der Funktion innerhalb des Rechtecks). Den Mittelwert schaetzen wir, indem
  # wir viele zufaellige Punkte gleichmaessig im Rechteck ziehen und dort die
  # Funktion auswerten.
  #
  #   rf_fit - randomForest-Objekt
  #   sl_max - Obergrenze des sl-Bereichs
  #   n_mc   - Anzahl der Monte-Carlo-Stichproben
  # Rueckgabe: numeric(1). Geschaetzte Normierungskonstante Z.
  estimateNormalizingConstantRF <- function(rf_fit, sl_max, n_mc = 20000) {
    mc_sl <- stats::runif(n_mc, 0, sl_max)
    mc_ta <- stats::runif(n_mc, -pi, pi)
    mc_vals <- evalDensitySurfaceRF(mc_sl, mc_ta, rf_fit)

    # Flaeche des Rechtecks [0, sl_max] x [-pi, pi]
    domain_area <- sl_max * 2 * pi

    mean(mc_vals) * domain_area
  }


  # Draw samples from the normalized RF surface via BayesianTools MCMC.
  #
  # Verwendet BayesianTools, um aus der (unnormierten) RF-Flaeche neue
  # (sl, ta)-Paare zu ziehen. BayesianTools erwartet eine Zielfunktion, die
  # eine LOG-Dichte zurueckgibt (nicht die Dichte selbst) - das ist Standard
  # bei MCMC-Verfahren, da log-Werte numerisch stabiler sind.
  #
  #   n          - gewuenschte Anzahl an Samples (nach Burn-in)
  #   rf_fit     - randomForest-Objekt
  #   sl_max     - Obergrenze des sl-Bereichs
  #   burn_in    - Anzahl der zu verwerfenden initialen Iterationen
  #   iterations - Gesamtzahl der MCMC-Iterationen; falls NULL, automatisch
  #                auf burn_in + n * 5 gesetzt
  #
  # Rueckgabe: eine Liste mit zwei Elementen:
  #   samples  - Matrix mit Spalten "sl" und "ta"
  #   mcmc_out - das vollstaendige BayesianTools-MCMC-Objekt, z.B. fuer
  #              BayesianTools::tracePlot() oder gelmanDiagnostics()
  bayesianToolsSampleDensitySurface <- function(n, rf_fit, sl_max,
                                                burn_in = 1000,
                                                iterations = NULL) {

    # Zielfunktion fuer BayesianTools: nimmt einen Parametervektor
    # par = c(sl, ta) entgegen und gibt einen einzelnen log-Dichtewert zurueck.
    log_density_fn <- function(par) {
      sl_val <- par[1]
      ta_val <- par[2]

      # Harte Grenzen: liegt der Vorschlag ausserhalb des gueltigen Bereichs,
      # ist die Wahrscheinlichkeit 0 -> log(0) = -Inf. BayesianTools lehnt
      # solche Vorschlaege dann automatisch ab.
      if (sl_val < 0 || sl_val > sl_max || ta_val < -pi || ta_val > pi) {
        return(-Inf)
      }

      dens <- evalDensitySurfaceRF(sl_val, ta_val, rf_fit)
      # + 1e-10 verhindert log(0) = -Inf an Stellen, wo der RF exakt 0 vorhersagt
      log(dens + 1e-10)
    }

    # createBayesianSetup() definiert das Optimierungs-/Sampling-Problem:
    # likelihood = unsere log-Dichte-Funktion, lower/upper = der Suchraum
    # (entspricht dem Definitionsbereich von sl und ta).
    bayesianSetup <- BayesianTools::createBayesianSetup(
      likelihood = log_density_fn,
      lower      = c(0, -pi),
      upper      = c(sl_max, pi)
    )

    if (is.null(iterations)) iterations <- burn_in + n * 5

    # sampler = "DEzs": Differential-Evolution-MCMC mit mehreren parallelen
    # Chains. Das ist robuster als ein einfaches Metropolis-Hastings, vor
    # allem weil Random-Forest-Vorhersagen stueckweise konstant sind (treppig)
    # und ein einfacher Sampler sich dort leicht "festfahren" kann.
    settings <- list(iterations = iterations, burnin = burn_in, message = FALSE)

    mcmc_out <- BayesianTools::runMCMC(bayesianSetup = bayesianSetup,
                                       sampler       = "DEzs",
                                       settings      = settings)

    # getSample() extrahiert die gezogenen Parameterwerte aus allen Chains,
    # der Burn-in-Bereich (start = burn_in + 1) wird dabei ausgeschlossen.
    samples_matrix <- BayesianTools::getSample(mcmc_out, start = burn_in + 1)
    colnames(samples_matrix) <- c("sl", "ta")

    # Falls mehr Samples vorhanden sind als benoetigt (n), zufaellig die
    # gewuenschte Anzahl auswaehlen.
    if (nrow(samples_matrix) > n) {
      keep <- sample(seq_len(nrow(samples_matrix)), n)
      samples_matrix <- samples_matrix[keep, , drop = FALSE]
    }

    # Falls WENIGER Samples vorhanden sind als benoetigt (n): BayesianTools'
    # DEzs-Sampler verwaltet Chains/Population-Groesse intern selbst, daher
    # entspricht die tatsaechliche Anzahl an Samples nach Abzug des Burn-in
    # nicht immer exakt "iterations - burn_in" - das passiert vor allem bei
    # kleinen angefragten n (z.B. n_control = 1). Wir gleichen das hier durch
    # Ziehen MIT Zuruecklegen (replace = TRUE) aus den vorhandenen Samples
    # aus, statt einen Fehler zu werfen. Das ist statistisch unproblematisch,
    # da die vorhandenen Samples bereits (nach Burn-in) aus der Zielverteilung
    # stammen - "mit Zuruecklegen ziehen" entspricht einem klassischen
    # Bootstrap und aendert die zugrunde liegende Verteilung nicht.
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


  # missing() muss VOR der naechsten Zeile (match.arg()) aufgerufen werden:
  # match.arg() bindet sl_transform neu (auf den ersten Default-Wert, falls
  # nichts uebergeben wurde), und danach laesst sich nicht mehr unterscheiden,
  # ob der Nutzer den Parameter explizit gesetzt hat oder der Default
  # einfach so "durchgereicht" wurde.
  sl_transform_was_set <- !missing(sl_transform)
  sl_transform <- match.arg(sl_transform)

  # --- Validierung (wie bisher) ---
  if (!inherits(x, c("steps_xy", "steps_xyt", "track_xy", "track_xyt"))) {
    stop("`x` must be an amt `steps_xy`, `steps_xyt`, `track_xy`, or `track_xyt` object.")}
  if (!is.numeric(n_control) || length(n_control) != 1L ||
      n_control != floor(n_control) || n_control < 1L) {
    stop("`n_control` must be a single positive integer.")}
  n_control <- as.integer(n_control)

  if (inherits(x, c("track_xy", "track_xyt")) &&
      !inherits(x, c("steps_xy", "steps_xyt"))) {
    x <- amt::steps(x)}
  x$step_id_ <- seq_len(nrow(x))
  estimator <- match.arg(estimator)

  # sl_transform wirkt nur im "density_advanced"-Zweig (siehe weiter unten).
  # Falls der Nutzer ihn trotzdem explizit gesetzt hat, aber einen anderen
  # estimator gewaehlt hat, wuerde der Parameter sonst stillschweigend
  # wirkungslos bleiben - das ist eine typische Quelle fuer "warum aendert
  # sich nichts?"-Verwirrung, deshalb hier eine explizite Warnung.
  if (sl_transform_was_set && estimator != "density_advanced") {
    warning(
      "`sl_transform` wird nur fuer `estimator = \"density_advanced\"` verwendet ",
      "und wird hier ignoriert, da `estimator = \"", estimator, "\"` gewaehlt wurde.",
      call. = FALSE
    )
  }

  # --- "traditional" (unveraendert) ---
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
    out$probability <- sl_prob * ta_prob
    class(out) <- c("random_steps_prop", class(out))
    return(out)}

  # Ab hier: "density" und "density_advanced" teilen sich die
  # Vorverarbeitung (gueltige Zeilen herausfiltern, sl_max bestimmen)
  valid   <- !is.na(x$ta_) & !is.na(x$direction_p) & !is.na(x$sl_)
  x_valid <- x[valid, ]
  sl_max  <- max(x_valid$sl_) * 1.5
  n_obs   <- nrow(x_valid)

  # --- "density" (KDE, unveraendert) ---
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

    obs_idx <- rep(seq_len(n_obs), each = n_control)
    abs_angle <- x_valid$direction_p[obs_idx] + ta_random
    x2_random <- x_valid$x1_[obs_idx] + sl_random * cos(abs_angle)
    y2_random <- x_valid$y1_[obs_idx] + sl_random * sin(abs_angle)

    random_tbl <- x_valid[obs_idx, ]
    random_tbl$x2_ <- x2_random
    random_tbl$y2_ <- y2_random
    random_tbl$sl_ <- sl_random
    random_tbl$ta_ <- ta_random
    random_tbl$case_ <- FALSE
    random_tbl$step_id_ <- rep(x_valid$step_id_, each = n_control)
    random_tbl$probability <- stats::predict(kde_fit, x = cbind(sl_random, ta_random))

    obs_tbl <- x_valid
    obs_tbl$case_ <- TRUE
    obs_tbl$probability <- stats::predict(kde_fit, x = cbind(x_valid$sl_, x_valid$ta_))

    core_cols <- c("step_id_", "case_", "probability", "x1_", "x2_", "y1_", "y2_", "sl_", "ta_")
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
    return(out)
  }

  # --- "density_advanced" (Random Forest + BayesianTools) ---
  if (estimator == "density_advanced") {

    # 1. Glatte (bzw. hier: stueckweise konstante) Dichte-Landschaft fitten
    rf_fit <- fitDensitySurfaceRF(x_valid$sl_, x_valid$ta_, sl_max,
                                  n_bins_sl    = rf_n_bins,
                                  n_bins_ta    = rf_n_bins,
                                  ntree        = rf_ntree,
                                  sl_transform = sl_transform)

    # 2. Normierungskonstante schaetzen (Integral der Flaeche = 1 erzwingen)
    Z <- estimateNormalizingConstantRF(rf_fit, sl_max)

    # 3. Random Steps aus der normierten Flaeche samplen
    n_total <- n_obs * n_control
    mh_result <- bayesianToolsSampleDensitySurface(n_total, rf_fit, sl_max,
                                                   burn_in = mcmc_burn_in)
    sl_random <- mh_result$samples[, "sl"]
    ta_random <- mh_result$samples[, "ta"]

    # 4. Endkoordinaten rekonstruieren (analog zu den anderen Branches)
    obs_idx <- rep(seq_len(n_obs), each = n_control)
    abs_angle <- x_valid$direction_p[obs_idx] + ta_random
    x2_random <- x_valid$x1_[obs_idx] + sl_random * cos(abs_angle)
    y2_random <- x_valid$y1_[obs_idx] + sl_random * sin(abs_angle)

    random_tbl <- x_valid[obs_idx, ]
    random_tbl$x2_ <- x2_random
    random_tbl$y2_ <- y2_random
    random_tbl$sl_ <- sl_random
    random_tbl$ta_ <- ta_random
    random_tbl$case_ <- FALSE
    random_tbl$step_id_ <- rep(x_valid$step_id_, each = n_control)
    random_tbl$probability <- evalDensitySurfaceRF(sl_random, ta_random, rf_fit) / Z

    obs_tbl <- x_valid
    obs_tbl$case_ <- TRUE
    obs_tbl$probability <- evalDensitySurfaceRF(x_valid$sl_, x_valid$ta_, rf_fit) / Z

    core_cols <- c("step_id_", "case_", "probability", "x1_", "x2_", "y1_", "y2_", "sl_", "ta_")
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

    # MCMC-Objekt als Attribut anhaengen, damit man spaeter Diagnostik
    # (Trace-Plots, Gelman-Rubin) durchfuehren kann, ohne die Funktion
    # erneut aufrufen zu muessen.
    attr(out, "mcmc_out") <- mh_result$mcmc_out

    return(out)
  }
}





