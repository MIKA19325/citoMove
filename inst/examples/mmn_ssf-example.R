library(cito)
library(citoMove)

# One habitat raster patch per step and two step covariates, 11 alternatives per stratum.
# The multi-modal model learns selection from the raster through a CNN branch and from the
# step covariates through a DNN branch, in one conditional-logit likelihood.
n_control = 10L
n_strata = 100
n = n_strata * (n_control + 1)

# habitat: the animal selects for the centre of the patch
X_cnn = array(runif(n * 5 * 5, -1, 1), dim = c(n, 1, 5, 5))
# step covariates: step length and turning angle
X_dnn = cbind(sl = rgamma(n, 2, 2), ta = runif(n, -pi, pi))

eta = 2 * apply(X_cnn[, 1, 3, 3, drop = FALSE], 1, sum) - 0.8 * X_dnn[, "sl"]
case = unlist(lapply(seq_len(n_strata), function(s) {
  i = ((s - 1) * (n_control + 1) + 1):(s * (n_control + 1))
  rmultinom(1, 1, exp(eta[i]) / sum(exp(eta[i])))[, 1]
}))

architecture = create_architecture(conv(n_kernels = 4L, kernel_size = c(3, 3)),
                                   linear(10, activation = "relu"))

model = mmn_ssf(case ~ cnn(X = X_cnn, architecture = architecture) +
                       dnn(~., data = X_dnn, hidden = c(20, 20)),
                dataList = list(case = case, X_cnn = X_cnn, X_dnn = X_dnn),
                n_control = n_control, epochs = 30L, lr = 0.05,
                plot = FALSE, verbose = FALSE)

# Rows must arrive grouped by stratum: the loss reshapes them to (n_strata, n_control + 1) and
# never reorders, so shuffle is forced off and the batch size is rounded to whole strata.
predict(model)[1:5]
