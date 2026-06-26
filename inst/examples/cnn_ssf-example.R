library(cito)
library(citoMove)
n = 100*11
X = array(runif(n*5*5*1, -1, 1), dim = c(n, 5, 5))
softmax = function(x) exp(x)/sum(exp(x))
YY = apply(matrix(sapply(1:n, function(i) sum((X[i,,] * diag(1, 5, 5)))*2 ), nrow = 11), 2, function(x) rmultinom(1, 1, prob = softmax(x)))
Y = as.vector(YY)

X = array(X, dim = c(n, 1, 5, 5))
architecture = create_architecture(conv(n_kernels = 1L, kernel_size = c(5,5)), linear(20, activation = "relu"))
m = cnn_ssf(X = X, Y = Y, density = rep(1, n), n_control = 10L, architecture= architecture, lr = 0.1, epochs = 500L)
